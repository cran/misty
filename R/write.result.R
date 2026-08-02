#' Write Results of a misty Object into an Excel file
#'
#' This function writes the results of a \code{misty.object}) into an Excel file.
#'
#  Currently the function supports result objects from the following functions:
#' \code{\link{blimp.bayes}}, \code{\link{boot.bs}}, \code{\link{ci.cor}},
#' \code{\link{ci.mean}}, \code{\link{ci.median}}, \code{\link{ci.prop}},
#' \code{\link{ci.var}}, \code{\link{ci.sd}}, code{\link{coeff.robust}},
#' \code{\link{coeff.std}}, \code{\link{cor.matrix}}, \code{\link{crosstab}},
#' \code{\link{descript}}, \code{\link{difftest.chibarsq}}, \code{\link{dominance.manual}},
#' \code{\link{dominance}}, \code{\link{effsize}}, \code{\link{freq}},
#' \code{\link{item.alpha}}, \code{\link{item.cfa}}, \code{\link{item.dfi}},
#' \code{\link{item.distract}}, \code{\link{item.invar}}, \code{\link{item.noninvar}},
#' \code{\link{item.omega}}, \code{\link{item.stats}}, \code{\link{mplus.bayes}},
#' \code{\link{multilevel.cfa}}, \code{\link{multilevel.cor}}, \code{\link{multilevel.descript}},
#' \code{\link{multilevel.fit}}, \code{\link{multilevel.invar}}, \code{\link{multilevel.omega}},
#' \code{\link{na.auxiliary}}, \code{\link{na.coverage}}, \code{\link{na.descript}},
#' \code{\link{na.pattern}}, \code{\link{mplus.lca.summa}}, \
#' \code{\link{summa}} and \code{\link{uniq}}
#'
#' @param x           misty object (\code{misty.object}) resulting from a misty
#'                    function supported by the \code{write.result} function (see
#'                    'Details').
#' @param file        a character string naming a file with or without file extension
#'                    '.xlsx', e.g., \code{"Results.xlsx"} or \code{"Results"}.
#' @param digits      an integer value indicating the number of decimal places
#'                    digits to be used for displaying results.
#' @param p.digits    an integer indicating the number of decimal places to be
#'                    used for displaying \emph{p}-values.
#' @param icc.digits  an integer indicating the number of decimal places to be
#'                    used for displaying intraclass correlation coefficients
#'                    (\code{multilevel.descript()} and \code{multilevel.icc()}
#'                    function).
#' @param r.digits    an integer value indicating the number of decimal places
#'                    to be used for displaying R-hat values, item-total correlation
#'                    standardized factor loadings, coefficient alpha, and
#'                    coefficient omega.
#' @param ess.digits  an integer value indicating the number of decimal places
#'                    to be used for displaying effective sample sizes.
#' @param mcse.digits an integer value indicating the number of decimal places
#'                    to be used for displaying Monte Carlo standard errors.
#' @param check       logical: if \code{TRUE} (default), argument specification
#'                    is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @export
#'
#' @examples
#' #————————————————————————————————————————————————————————————————————————————
#  # Example 1: item.cfa() function
#'
#' # Load data set "HolzingerSwineford1939" in the lavaan package
#' data("HolzingerSwineford1939", package = "lavaan")
#'
#' result <- item.cfa(HolzingerSwineford1939[, c("x1", "x2", "x3")], output = FALSE)
#' write.result(result, "CFA.xlsx")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Example 2: multilevel.descript() function
#'
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' result <- multilevel.descript(y1:y3, data = Demo.twolevel, cluster = "cluster",
#'                               output = FALSE)
#' write.result(result, "Multilevel_Descript.xlsx")
write.result <- function(x, file = "Results.xlsx", digits = x$args$digits,
                         p.digits = x$args$p.digits, icc.digits = x$args$icc.digits,
                         r.digits = x$args$r.digits, ess.digits = x$args$ess.digits,
                         mcse.digits = x$args$mcse.digits, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing, NULL, or misty object
  if (isTRUE(missing(x) || is.null(x) || !inherits(x, "misty.object"))) { stop("Please specify a misty object for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is supported by the function
  if (isTRUE(!x$type %in% c("aov.b", "aov.w", "blimp.bayes", "boot.bs", "ci.cor", "ci.mean", "ci.median", "ci.prop", "ci.var", "ci.sd", "coeff.robust", "coeff.std", "cor.matrix", "crosstab", "descript", "difftest.chibarsq", "dominance.manual", "dominance", "effsize", "freq", "item.alpha", "item.cfa", "item.dfi", "item.distract", "item.invar", "item.noninvar", "item.omega", "item.stats", "modcomp", "mplus.bayes", "multilevel.alpha", "multilevel.cfa", "multilevel.cor", "multilevel.descript", "multilevel.fit", "multilevel.invar", "multilevel.omega", "na.auxiliary", "na.coverage", "na.descript", "na.pattern", "mplus.lca.summa", "robust.lmer", "summa", "test.levene", "test.t", "test.welch", "test.z", "uniq"))) { stop("This type of misty object is not supported by the write.result() function.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  # Write object
  write.object <- x$result

  # 'write' argument
  write <- x$args$print

  #_____________________________________________________________________________
  #
  # Between-Subject Analysis of Variance, aov.b() ------------------------------
  switch(x$type, aov.b = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Round and Format ####

    #—————————————————————————————————————— #
    ### Round ####

    write.object[["descript"]][, c("m", "low", "upp", "sd", "skew", "kurt")] <- round(write.object[["descript"]][, c("m", "low", "upp", "sd", "skew", "kurt")], digits = digits)

    write.object[["test"]][, c("sum.sq", "mean.sq", "F", "eta.sq", "omega.sq")] <- round(write.object[["test"]][, c("sum.sq", "mean.sq", "F", "eta.sq", "omega.sq")], digits = digits)
    write.object[["test"]][, "pval"] <- round(write.object[["test"]][, "pval"], digits = p.digits)

    write.object[["posthoc"]][, c("m.diff", "m.low", "m.upp", "d", "d.low", "d.upp")] <- round(write.object[["posthoc"]][, c("m.diff", "m.low", "m.upp", "d", "d.low", "d.upp")], digits = digits)
    write.object[["posthoc"]][, "pval"] <- round(write.object[["posthoc"]][, "pval"], digits = p.digits)

    #—————————————————————————————————————— #
    ### Column Names ####

    colnames(write.object[["descript"]]) <- c("Group", "n", "nNA", "M", "Low", "Upp", "SD", "Skew", "Kurt")
    colnames(write.object[["test"]]) <- c("Source", "SumSq", "df", "MeanSq",  "F", "p", "eta.sq", "omega.sq")
    colnames(write.object[["posthoc"]]) <- c("Group1", "Group2", "M.diff", "Low", "Upp", "p", "d", "Low", "Upp")

    #—————————————————————————————————————— #
    ### Remove Result ####

    # Descriptive statistics
    if (isTRUE(!x$args$descript)) { write.object[["descript"]] <- NULL }

    # Effect sizes
    if (isTRUE(!x$args$effsize)) {

      write.object[["test"]] <- write.object[["test"]][, -which(colnames(write.object[["test"]]) %in% c("eta.sq", "omega.sq"))]

      write.object[["posthoc"]] <- write.object[["posthoc"]][, -which(colnames(write.object[["posthoc"]]) %in% c("d", "Low", "Upp"))]

    }

    # Post-hoc test
    if (isTRUE(!x$args$posthoc)) { write.object[["posthoc"]] <- NULL }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Write Object ####

    write.object <- list(ANOVA = write.object$test, Descript = write.object$descript, PostHoc = write.object$posthoc)

  #_____________________________________________________________________________
  #
  # Repeated Measures Analysis of Variance, aov.w() ----------------------------
  }, aov.w = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Round and Format ####

    #—————————————————————————————————————— #
    ### Round ####

    # Descriptive statistics
    write.object[["descript"]][, c("m", "low", "upp", "sd", "skew", "kurt")] <- round(write.object[["descript"]][, c("m", "low", "upp", "sd", "skew", "kurt")], digits = digits)

    # Box Index of Sphericity
    write.object[["epsilon"]][, "epsilon"] <- round(write.object[["epsilon"]][, "epsilon"], digits = digits)

    # ANOVA tables
    write.object[["test"]][["none"]][, c("sum.sq", "mean.sq", "F", "eta.sq", "eta.sq.p", "omega.sq", "omega.sq.p")] <- round(write.object[["test"]][["none"]][, c("sum.sq", "mean.sq", "F", "eta.sq", "eta.sq.p", "omega.sq", "omega.sq.p")], digits = digits)
    write.object[["test"]][["none"]][, "p"] <- round(write.object[["test"]][["none"]][, "p"], digits = p.digits)

    write.object[["test"]][["gg"]][, c("sum.sq", "df", "mean.sq", "F", "eta.sq", "eta.sq.p", "omega.sq", "omega.sq.p")] <- round(write.object[["test"]][["gg"]][, c("sum.sq", "df", "mean.sq", "F", "eta.sq", "eta.sq.p", "omega.sq", "omega.sq.p")], digits = digits)
    write.object[["test"]][["gg"]][, "p"] <- round(write.object[["test"]][["gg"]][, "p"], digits = p.digits)

    write.object[["test"]][["hf"]][, c("sum.sq", "df", "mean.sq", "F", "eta.sq", "eta.sq.p", "omega.sq", "omega.sq.p")] <- round(write.object[["test"]][["hf"]][, c("sum.sq", "df", "mean.sq", "F", "eta.sq", "eta.sq.p", "omega.sq", "omega.sq.p")], digits = digits)
    write.object[["test"]][["hf"]][, "p"] <- round(write.object[["test"]][["hf"]][, "p"], digits = p.digits)

    write.object[["test"]][["lb"]][, c("sum.sq", "df", "mean.sq", "F", "eta.sq", "eta.sq.p", "omega.sq", "omega.sq.p")] <- round(write.object[["test"]][["lb"]][, c("sum.sq", "df", "mean.sq", "F", "eta.sq", "eta.sq.p", "omega.sq", "omega.sq.p")], digits = digits)
    write.object[["test"]][["lb"]][, "p"] <- round(write.object[["test"]][["lb"]][, "p"], digits = p.digits)

    # Post hoc tests
    write.object[["posthoc"]][, c("m.diff", "t", "d", "d.low", "d.upp")] <- round(write.object[["posthoc"]][, c("m.diff", "t", "d", "d.low", "d.upp")], digits = digits)
    write.object[["posthoc"]][, "p"] <- round(write.object[["posthoc"]][, "p"], digits = p.digits)

    #—————————————————————————————————————— #
    ### Column names ####

    colnames(write.object[["descript"]]) <- c("Variable", "n", "nNA", "M", "Low", "Upp", "SD", "Skew", "Kurt")
    colnames(write.object[["epsilon"]]) <- c("Box Index of Sphericity", "epsilon")
    colnames(write.object[["test"]][["none"]]) <- c("Source", "SS", "df", "MSS", "F", "p", "eta2", "eta2p", "omega2", "omega2p")
    colnames(write.object[["test"]][["gg"]]) <- c("Source", "SS", "df", "MSS", "F", "p", "eta2", "eta2p", "omega2", "omega2p")
    colnames(write.object[["test"]][["hf"]]) <- c("Source", "SS", "df", "MSS", "F", "p", "eta2", "eta2p", "omega2", "omega2p")
    colnames(write.object[["test"]][["lb"]]) <- c("Source", "SS", "df", "MSS", "F", "p", "eta2", "eta2p", "omega2", "omega2p")
    colnames(write.object[["posthoc"]]) <- c("Variable1", "Variable2", "M.diff", "t", "df", "p", "d", "Low", "Upp")

    #—————————————————————————————————————— #
    ### Remove Result ####

    # Descriptive statistics
    if (isTRUE(!x$args$descript)) { write.object[["descript"]] <- NULL }

    # Effect sizes
    if (isTRUE(!x$args$effsize)) {

      write.object[["test"]][["none"]] <- write.object[["test"]][["none"]][, -which(colnames(write.object[["test"]][["none"]]) %in% c("eta2", "eta2p", "omega2", "omega2p"))]
      write.object[["test"]][["gg"]] <- write.object[["test"]][["gg"]][, -which(colnames(write.object[["test"]][["gg"]]) %in% c("eta2", "eta2p", "omega2", "omega2p"))]
      write.object[["test"]][["hf"]] <- write.object[["test"]][["hf"]][, -which(colnames(write.object[["test"]][["hf"]]) %in% c("eta2", "eta2p", "omega2", "omega2p"))]
      write.object[["test"]][["lb"]] <- write.object[["test"]][["lb"]][, -which(colnames(write.object[["test"]][["lb"]]) %in% c("eta2", "eta2p", "omega2", "omega2p"))]

      write.object[["posthoc"]] <- write.object[["posthoc"]][, -which(colnames(write.object[["posthoc"]]) %in% c("d", "Low", "Upp"))]

    }

    # Sphericity correction
    if (isTRUE(!"none" %in% write)) { write.object[["test"]][["none"]] <- NULL }
    if (isTRUE(!"GG" %in% write)) { write.object[["test"]][["gg"]] <- NULL }
    if (isTRUE(!"HF" %in% write)) { write.object[["test"]][["hf"]] <- NULL }
    if (isTRUE(!"LB" %in% write)) { write.object[["test"]][["lb"]] <- NULL }

    # Post-hoc test
    if (isTRUE(!x$args$posthoc)) { write.object[["posthoc"]] <- NULL }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Write Object ####

    write.object <- list(Epsilon =  write.object[["epsilon"]],
                         "ANOVA-None" = write.object[["test"]][["none"]], "ANOVA-HF" = write.object[["test"]][["hf"]], "ANOVA-GG" = write.object[["test"]][["gg"]], "ANOVA-LB" = write.object[["test"]][["lb"]], Descript = write.object$descript,
                         PostHoc = write.object$posthoc)

  #_____________________________________________________________________________
  #
  # Blimp Summary Measures, blimp.bayes() --------------------------------------
  }, blimp.bayes = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Round ####

    # digits
    print.round <- c("m", "med", "map", "sd", "mad", "skew", "kurt", "eti.low", "eti.upp", "hdi.low", "hdi.upp")
    write.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(write.object[, y]), round(write.object[, y], digits = digits), NA))

    # r.digits
    write.object[, "rhat"] <- ifelse(!is.na(write.object[, "rhat"]), round(write.object[, "rhat"], digits = r.digits), NA)

    # ess.digits
    write.object[, "b.ess"] <- ifelse(!is.na(write.object[, "b.ess"]), round(write.object[, "b.ess"], digits = ess.digits), NA)
    write.object[, "t.ess"] <- ifelse(!is.na(write.object[, "t.ess"]), round(write.object[, "t.ess"], digits = ess.digits), NA)

    # mcse.digits
    write.object[, "b.mcse"] <- ifelse(!is.na(write.object[, "b.mcse"]), round(write.object[, "b.mcse"], digits = mcse.digits), NA)
    write.object[, "t.mcse"] <- ifelse(!is.na(write.object[, "t.mcse"]), round(write.object[, "t.mcse"], digits = mcse.digits), NA)

    # p.digits
    write.object[, "pd"] <- ifelse(!is.na(write.object[, "pd"]), round(write.object[, "pd"], digits = p.digits), NA)
    write.object[, "rope"] <- ifelse(!is.na(write.object[, "rope"]), round(write.object[, "rope"], digits = p.digits), NA)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Variable Names ####

    colnames(write.object) <- c("Param", "L1", "L2", "L3", "M", "Med", "MAP", "SD", "MAD", "Skew", "Kurt", "ETI.Low", "ETI.Upp", "HDI.Low", "HDI.Upp", "R-hat", "B.ESS", "T.ESS", "B.MCSE", "T.MCSE", "pd", "ROPE")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Select Statistical Measures and Add Parameters ####

    # Print statistics
    print <- misty::rec(write, spec = "'m' = 'M'; 'med' = 'Med'; 'map' = 'MAP'; 'sd' = 'SD'; 'mad' = 'MAD'; 'skew' = 'Skew'; 'kurt' = 'Kurt'; 'rhat' = 'R-hat'; 'b.ess' = 'B.ESS'; 't.ess' = 'T.ESS'; 'b.mcse' = 'B.MCSE'; 't.mcse' = 'T.MCSE'; 'rope' = 'ROPE'")

    if (isTRUE("eti" %in% print)) { print <- c(print, c("ETI.Low", "ETI.Upp")) }
    if (isTRUE("hdi" %in% print)) { print <- c(print, c("HDI.Low", "HDI.Upp")) }

    # Sort
    print <- intersect(c("M", "Med", "MAP", "SD", "MAD", "Skew", "Kurt", "ETI.Low", "ETI.Upp", "HDI.Low", "HDI.Upp", "R-hat", "B.ESS", "T.ESS", "B.MCSE", "T.MCSE"), print)

    # Select
    write.object <- data.frame(write.object[, c(1L:4L)], write.object[, print, drop = FALSE], stringsAsFactors = FALSE, check.names = FALSE)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Note ####

    note <- NULL

    # R-hat
    if (isTRUE("R-hat" %in% print)) {

      if (isTRUE(x$args$fold)) {

        note <- rbind(note, data.frame("Maximum of Rank-Normalized (Folded-)Split R-hat", fix.empty.names = FALSE))

      } else {

        if (isTRUE(x$args$rank)) {

          if (isTRUE(x$args$split)) {

            note <- rbind(note, data.frame("Rank-Normalizsed Split R-hat", fix.empty.names = FALSE))

          } else {

            note <- rbind(note, data.frame("Rank-Normalizsed R-hat", fix.empty.names = FALSE))

          }

        } else {

          if (isTRUE(x$args$split)) {

            note <- rbind(note, data.frame("Traditional Split R-hat", fix.empty.names = FALSE))

          } else {

            note <- rbind(note, data.frame("Traditional R-hat", fix.empty.names = FALSE))

          }

        }

      }

    }

    # ROPE
    if (isTRUE(!is.null(x$args$rope))) {

      if (isTRUE("ROPE" %in% print)) {

        note <- rbind(note, data.frame(paste0("Region of Practical Equivalence (ROPE): [", x$args$rope[1L], ", ", x$args$rope[2L], "]"), fix.empty.names = FALSE))

      } else {

        note <- rbind(note, data.frame(paste0("Region of Practical Equivalence (ROPE): [", x$args$rope[1L], ", ", x$args$rope[2L], "]"), fix.empty.names = FALSE))

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Write Object ####

    if (isTRUE(!is.null(note))) { write.object <- list(Summary = write.object, Note = note) }

  #_____________________________________________________________________________
  #
  # Bollen-Stine Bootstrap with Incomplete Data, boot.bs() ---------------------
  }, boot.bs = {

    # Round
    write.object[, "chisq"] <- round(write.object[, "chisq"], digits = digits)
    write.object[, "p"] <- round(write.object[, "p"], digits = p.digits)
    write.object[, "boot.p"] <- round(write.object[, "boot.p"], digits = p.digits)

    # Column
    colnames(write.object) <- c("R", "nNA", "Chisq", "df", "p", "pBoot")

  #_____________________________________________________________________________
  #
  # Confidence Interval for the Correlation Coefficient, ci.cor() --------------
  }, ci.cor = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No grouping ####

    if (isTRUE(is.null(x$data$group) && is.null(x$data$split))) {

      #—————————————————————————————————————— #
      ### Round ####

      write.object[, !sapply(write.object, is.character)] <- sapply(write.object[, !sapply(write.object, is.character)], round, digits = digits)

      #—————————————————————————————————————— #
      ### Remove Duplicated Values ####

      write.object[duplicated(write.object$var1) , "var1"] <- ""

      #—————————————————————————————————————— #
      ### Column Names ####

      switch(x$args$method, "pearson" = {

        colnames(write.object) <- c("Variable 1", "Variable 2", "n", "nNA", "pNA", "Skew1", "Kurt1", "Skew2", "Kurt2", "r", "Low", "Upp")

      }, "spearman" = {

        colnames(write.object) <- c("Variable 1", "Variable 2", "n", "nNA", "pNA", "Skew1", "Kurt1", "Skew2", "Kurt2", "rs", "Low", "Upp")

      }, "kendall-b" = {

        colnames(write.object) <- c("Variable 1", "Variable 2", "n", "nNA", "pNA", "Skew1", "Kurt1", "Skew2", "Kurt2", "Tau-b", "Low", "Upp")

      }, "kendall-c" = {

        colnames(write.object) <- c("Variable 1", "Variable 2", "n", "nNA", "pNA", "Skew1", "Kurt1", "Skew2", "Kurt2", "Tau-c", "Low", "Upp")

      })

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Grouping ####

    } else if (isTRUE(!is.null(x$data$group) && is.null(x$data$split))) {

      #—————————————————————————————————————— #
      ### Round ####

      write.object[, !sapply(write.object, is.character)] <- sapply(write.object[, !sapply(write.object, is.character)], round, digits = digits)

      #—————————————————————————————————————— #
      ### Format ####

      # Remove duplicated values
      write.object[duplicated(paste(write.object$group, write.object$var1)) , "var1"] <- ""
      write.object[duplicated(write.object$group) , "group"] <- ""


      #—————————————————————————————————————— #
      ### Column Names ####

      switch(x$args$method, "pearson" = {

        colnames(write.object) <- c("Group", "Variable 1", "Variable 2", "n", "nNA", "pNA", "Skew1", "Kurt1", "Skew2", "Kurt2", "r", "Low", "Upp")

      }, "spearman" = {

        colnames(write.object) <- c("Group", "Variable 1", "Variable 2", "n", "nNA", "pNA", "Skew1", "Kurt1", "Skew2", "Kurt2", "rs", "Low", "Upp")

      }, "kendall-b" = {

        colnames(write.object) <- c("Group", "Variable 1", "Variable 2", "n", "nNA", "pNA", "Skew1", "Kurt1", "Skew2", "Kurt2", "Tau-b", "Low", "Upp")

      }, "kendall-c" = {

        colnames(write.object) <- c("Group", "Variable 1", "Variable 2", "n", "nNA", "pNA", "Skew1", "Kurt1", "Skew2", "Kurt2", "Tau-c", "Low", "Upp")

      })

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Split ####

    } else if (isTRUE(!is.null(x$data$split))) {

      #—————————————————————————————————————— #
      ### No Grouping ####

      if (isTRUE(is.null(x$data$group))) {

        for (i in names(write.object)) {

          #### Round ####
          write.object[[i]][, !sapply(write.object[[i]], is.character)] <- sapply(write.object[[i]][, !sapply(write.object[[i]], is.character)], round, digits = digits)

          #### Remove Duplicated Values ####
          write.object[[i]][duplicated(write.object[[i]]$var1) , "var1"] <- ""

          #### Column Names ####
          switch(x$args$method, "pearson" = {

            colnames(write.object[[i]]) <- c("Variable 1", "Variable 2", "n", "nNA", "pNA", "Skew1", "Kurt1", "Skew2", "Kurt2", "r", "Low", "Upp")

          }, "spearman" = {

            colnames(write.object[[i]]) <- c("Variable 1", "Variable 2", "n", "nNA", "pNA", "Skew1", "Kurt1", "Skew2", "Kurt2", "rs", "Low", "Upp")

          }, "kendall-b" = {

            colnames(write.object[[i]]) <- c("Variable 1", "Variable 2", "n", "nNA", "pNA", "Skew1", "Kurt1", "Skew2", "Kurt2", "Tau-b", "Low", "Upp")

          }, "kendall-c" = {

            colnames(write.object[[i]]) <- c("Variable 1", "Variable 2", "n", "nNA", "pNA", "Skew1", "Kurt1", "Skew2", "Kurt2", "Tau-c", "Low", "Upp")

          })

        }

      #—————————————————————————————————————— #
      ### Grouping ####

      } else {

        for (i in names(write.object)) {

          #### Round ####
          write.object[[i]][, !sapply(write.object[[i]], is.character)] <- sapply(write.object[[i]][, !sapply(write.object[[i]], is.character)], round, digits = digits)

          #### Remove Duplicated Values ####
          write.object[[i]][duplicated(paste(write.object[[i]]$group, write.object$var1)) , "var1"] <- ""
          write.object[[i]][duplicated(write.object[[i]]$group) , "group"] <- ""

          #### Column Names ####
          switch(x$args$method, "pearson" = {

            colnames(write.object[[i]]) <- c("Group", "Variable 1", "Variable 2", "n", "nNA", "pNA", "Skew1", "Kurt1", "Skew2", "Kurt2", "r", "Low", "Upp")

          }, "spearman" = {

            colnames(write.object[[i]]) <- c("Group", "Variable 1", "Variable 2", "n", "nNA", "pNA", "Skew1", "Kurt1", "Skew2", "Kurt2", "rs", "Low", "Upp")

          }, "kendall-b" = {

            colnames(write.object[[i]]) <- c("Group", "Variable 1", "Variable 2", "n", "nNA", "pNA", "Skew1", "Kurt1", "Skew2", "Kurt2", "Tau-b", "Low", "Upp")

          }, "kendall-c" = {

            colnames(write.object[[i]]) <- c("Group", "Variable 1", "Variable 2", "n", "nNA", "pNA", "Skew1", "Kurt1", "Skew2", "Kurt2", "Tau-c", "Low", "Upp")

          })

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Note ####

    #—————————————————————————————————————— #
    ### No Bootstrapping ####

    if (isTRUE(x$args$boot == "none")) {

      note <- data.frame(c("Correlation Coefficient:", "Alternative Hypothesis:", "Confidence Level:", "Adjustment Method:", "Standard Error:"),
                         c(switch(x$args$method,
                                  "pearson" = "Pearson product-moment correlation coefficient",
                                  "spearman" = "Spearman's rank-order correlation coefficient",
                                  "kendall-b" = "Kendall's Tau-b correlation coefficient",
                                  "kendall-c" = "Kendall-Stuart's Tau-c correlation coefficient"),
                           x$args$alternative, x$args$conf.level,
                           switch(x$args$adjust,
                                  "none" = "Without non-normality adjustment",
                                  "joint" = "Non-normality adjustment via sample joint moments method",
                                  "approx" = "Non-normality adjustment via approximate distribution method"),
                           switch(x$args$se,
                                  "fisher" = "Fisher (1921) standard error",
                                  "fieller" = "Fieller et al. (1957) standard error",
                                  "bonett" = "Bonett and Wright (2000) standard error",
                                  "rin" = "Rank-based inverse normal transformation")),
                         fix.empty.names = FALSE)

    #—————————————————————————————————————— #
    ### Note Bootstrapping ####

    } else {

      note <- data.frame(c("Correlation Coefficient:", "Alternative Hypothesis:", "Confidence Level:", "Bootstrap Method:", "Replications:"),
                         c(switch(x$args$method,
                                  "pearson" = "Pearson product-moment correlation coefficient",
                                  "spearman" = "Spearman's rank-order correlation coefficient",
                                  "kendall-b" = "Kendall's Tau-b correlation coefficient",
                                  "kendall-c" = "Kendall-Stuart's Tau-c correlation coefficient"),
                           x$args$alternative, x$args$conf.level,
                           switch(x$args$boot,
                                  "norm" = "Bias-corrected normal approximation bootstrap CI",
                                  "basic" = "Basic bootstrap CI",
                                  "perc" = "Percentile bootstrap CI",
                                  "bc" = "Bias-corrected (BC) percentile bootstrap CI",
                                  "bca" = "Bias-corrected and accelerated (BCa) bootstrap CI"),
                           x$args$R),
                         fix.empty.names = FALSE)

    }

    #—————————————————————————————————————— #
    ### Write Object ####

    if (isTRUE(is.data.frame(write.object))) {

      write.object <- list("CI Cor" = write.object, Note = note)

    } else {

      write.object <- append(write.object, list(Note = note))

    }

  #_____________________________________________________________________________
  #
  # Confidence Interval for the Mean, ci.mean() --------------------------------
  }, ci.mean = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No grouping ####

    if (isTRUE(is.null(x$data$group) && is.null(x$data$split))) {

      #—————————————————————————————————————— #
      ### Round ####

      write.object[, !sapply(write.object, is.character)] <- sapply(write.object[, !sapply(write.object, is.character)], round, digits = digits)

      #—————————————————————————————————————— #
      ### Column Names ####

      colnames(write.object) <- c("Variable", "n", "nNA", "pNA", "SD", "Skew", "Kurt", "M", "Low", "Upp")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Grouping ####

    } else if (isTRUE(!is.null(x$data$group) && is.null(x$data$split))) {

      #—————————————————————————————————————— #
      ### Round ####

      write.object[, !sapply(write.object, is.character)] <- sapply(write.object[, !sapply(write.object, is.character)], round, digits = digits)

      #—————————————————————————————————————— #
      ### Remove Duplicated Values ####

      write.object[duplicated(write.object$group) , "group"] <- ""

      #—————————————————————————————————————— #
      ### Column Names ####

      colnames(write.object) <- c("Group", "Variable", "n", "nNA", "pNA", "SD", "Skew", "Kurt", "M", "Low", "Upp")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Split ####

    } else if (isTRUE(!is.null(x$data$split))) {

      #—————————————————————————————————————— #
      ### No Grouping ####

      if (isTRUE(is.null(x$data$group))) {

        for (i in names(write.object)) {

          #### Round ####
          write.object[[i]][, !sapply(write.object[[i]], is.character)] <- sapply(write.object[[i]][, !sapply(write.object[[i]], is.character)], round, digits = digits)

          #### Column Names ####
          colnames(write.object[[i]]) <- c("Variable", "n", "nNA", "pNA", "SD", "Skew", "Kurt", "M", "Low", "Upp")

        }

      #—————————————————————————————————————— #
      ### Grouping ####

      } else {

        for (i in names(write.object)) {

          #### Round ####
          write.object[[i]][, !sapply(write.object[[i]], is.character)] <- sapply(write.object[[i]][, !sapply(write.object[[i]], is.character)], round, digits = digits)

          #### Remove Duplicated Values ####
          write.object[[i]][duplicated(write.object[[i]]$group) , "group"] <- ""

          #### Column Names ####
          colnames(write.object[[i]]) <- c("Group", "Variable", "n", "nNA", "pNA", "SD", "Skew", "Kurt", "M", "Low", "Upp")

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Note Bootstrapping ####

    if (isTRUE(x$args$boot != "none")) {

      note <- data.frame(c("Alternative Hypothesis:", "Confidence Level:", "Bootstrap Method:", "Replications:"),
                         c(x$args$alternative, x$args$conf.level,
                           switch(x$args$boot,
                                  "norm" = "Bias-corrected normal approximation bootstrap CI",
                                  "basic" = "Basic bootstrap CI",
                                  "stud" = "Studentized bootstrap CI",
                                  "perc" = "Percentile bootstrap CI",
                                  "bc" = "Bias-corrected (BC) percentile bootstrap CI",
                                  "bca" = "Bias-corrected and accelerated (BCa) bootstrap CI"),
                           x$args$R),
                         fix.empty.names = FALSE)

      #—————————————————————————————————————— #
      ### Write Object ####

      if (isTRUE(is.data.frame(write.object))) {

        write.object <- list("CI Mean" = write.object, Note = note)

      } else {

        write.object <- append(write.object, list(Note = note))

      }

    } else {

      if (isTRUE(is.data.frame(write.object))) {

        write.object <- list("CI Mean" = write.object)

      }

    }

  #_____________________________________________________________________________
  #
  # Confidence Interval for the Median, ci.median() ----------------------------
  }, ci.median = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No grouping ####

    if (isTRUE(is.null(x$data$group) && is.null(x$data$split))) {

      #—————————————————————————————————————— #
      ### Round ####

      write.object[, !sapply(write.object, is.character)] <- sapply(write.object[, !sapply(write.object, is.character)], round, digits = digits)

      #—————————————————————————————————————— #
      ### Column Names ####

      colnames(write.object) <- c("Variable", "n", "nNA", "pNA", "SD", "IQR", "Skew", "Kurt", "Med", "Low", "Upp")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Grouping ####

    } else if (isTRUE(!is.null(x$data$group) && is.null(x$data$split))) {

      #—————————————————————————————————————— #
      ### Round ####

      write.object[, !sapply(write.object, is.character)] <- sapply(write.object[, !sapply(write.object, is.character)], round, digits = digits)

      #—————————————————————————————————————— #
      ### Remove Duplicated Values ####

      write.object[duplicated(write.object$group) , "group"] <- ""

      #—————————————————————————————————————— #
      ### Column Names ####

      colnames(write.object) <- c("Group", "Variable", "n", "nNA", "pNA", "SD", "IQR", "Skew", "Kurt", "Med", "Low", "Upp")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Split ####

    } else if (isTRUE(!is.null(x$data$split))) {

      #—————————————————————————————————————— #
      ### No Grouping ####

      if (isTRUE(is.null(x$data$group))) {

        for (i in names(write.object)) {

          #### Round ####
          write.object[[i]][, !sapply(write.object[[i]], is.character)] <- sapply(write.object[[i]][, !sapply(write.object[[i]], is.character)], round, digits = digits)

          #### Column Names ####
          colnames(write.object[[i]]) <- c("Variable", "n", "nNA", "pNA", "SD", "IQR", "Skew", "Kurt", "Med", "Low", "Upp")

        }

      #—————————————————————————————————————— #
      ### Grouping ####

      } else {

        for (i in names(write.object)) {

          #### Round ####
          write.object[[i]][, !sapply(write.object[[i]], is.character)] <- sapply(write.object[[i]][, !sapply(write.object[[i]], is.character)], round, digits = digits)

          #### Remove Duplicated Values ####
          write.object[[i]][duplicated(write.object[[i]]$group) , "group"] <- ""

          #### Column Names ####
          colnames(write.object[[i]]) <- c("Group", "Variable", "n", "nNA", "pNA", "SD", "IQR", "Skew", "Kurt", "Med", "Low", "Upp")

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Note Bootstrapping ####

    if (isTRUE(x$args$boot != "none")) {

      note <- data.frame(c("Alternative Hypothesis:", "Confidence Level:", "Bootstrap Method:", "Replications:"),
                         c(x$args$alternative, x$args$conf.level,
                           switch(x$args$boot,
                                  "norm" = "Bias-corrected normal approximation bootstrap CI",
                                  "basic" = "Basic bootstrap CI",
                                  "stud" = "Studentized bootstrap CI",
                                  "perc" = "Percentile bootstrap CI",
                                  "bc" = "Bias-corrected (BC) percentile bootstrap CI",
                                  "bca" = "Bias-corrected and accelerated (BCa) bootstrap CI"),
                           x$args$R),
                         fix.empty.names = FALSE)

      #—————————————————————————————————————— #
      ### Write Object ####

      if (isTRUE(is.data.frame(write.object))) {

        write.object <- list("CI Median" = write.object, Note = note)

      } else {

        write.object <- append(write.object, list(Note = note))

      }

    } else {

      if (isTRUE(is.data.frame(write.object))) {

        write.object <- list("CI Median" = write.object)

      }

    }

  #_____________________________________________________________________________
  #
  # Confidence Interval for the Proportion, ci.prop() --------------------------
  }, ci.prop = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No grouping ####

    if (isTRUE(is.null(x$data$group) && is.null(x$data$split))) {

      #—————————————————————————————————————— #
      ### Round ####

      write.object[, !sapply(write.object, is.character)] <- sapply(write.object[, !sapply(write.object, is.character)], round, digits = digits)

      #—————————————————————————————————————— #
      ### Column Names ####

      colnames(write.object) <- c("Variable", "n", "nNA", "pNA", "Freq", "Prop", "Low", "Upp")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Grouping ####

    } else if (isTRUE(!is.null(x$data$group) && is.null(x$data$split))) {

      #—————————————————————————————————————— #
      ### Round ####

      write.object[, !sapply(write.object, is.character)] <- sapply(write.object[, !sapply(write.object, is.character)], round, digits = digits)

      #—————————————————————————————————————— #
      ### Remove Duplicated Values ####

      write.object[duplicated(write.object$group) , "group"] <- ""

      #—————————————————————————————————————— #
      ### Column Names ####

      colnames(write.object) <- c("Group", "Variable", "n", "nNA", "pNA", "Freq", "Prop", "Low", "Upp")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Split ####

    } else if (isTRUE(!is.null(x$data$split))) {

      #—————————————————————————————————————— #
      ### No Grouping ####

      if (isTRUE(is.null(x$data$group))) {

        for (i in names(write.object)) {

          #### Round ####
          write.object[[i]][, !sapply(write.object[[i]], is.character)] <- sapply(write.object[[i]][, !sapply(write.object[[i]], is.character)], round, digits = digits)

          #### Column Names ####
          colnames(write.object[[i]]) <- c("Variable", "n", "nNA", "pNA", "Freq", "Prop", "Low", "Upp")

        }

      #—————————————————————————————————————— #
      ### Grouping ####

      } else {

        for (i in names(write.object)) {

          #### Round ####
          write.object[[i]][, !sapply(write.object[[i]], is.character)] <- sapply(write.object[[i]][, !sapply(write.object[[i]], is.character)], round, digits = digits)

          #### Remove Duplicated Values ####
          write.object[[i]][duplicated(write.object[[i]]$group) , "group"] <- ""

          #### Column Names ####
          colnames(write.object[[i]]) <- c("Group", "Variable", "n", "nNA", "pNA", "Freq", "Prop", "Low", "Upp")

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Note Bootstrapping ####

    if (isTRUE(x$args$boot != "none")) {

      note <- data.frame(c("Alternative Hypothesis:", "Confidence Level:", "Bootstrap Method:", "Replications:"),
                         c(x$args$alternative, x$args$conf.level,
                           switch(x$args$boot,
                                  "perc" = "Percentile bootstrap CI",
                                  "bc" = "Bias-corrected (BC) percentile bootstrap CI",
                                  "bca" = "Bias-corrected and accelerated (BCa) bootstrap CI"),
                           x$args$R),
                         fix.empty.names = FALSE)

      #—————————————————————————————————————— #
      ### Write Object ####

      if (isTRUE(is.data.frame(write.object))) {

        write.object <- list("CI Prop" = write.object, Note = note)

      } else {

        write.object <- append(write.object, list(Note = note))

      }

    } else {

      if (isTRUE(is.data.frame(write.object))) {

        write.object <- list("CI Prop" = write.object)

      }

    }

  #_____________________________________________________________________________
  #
  # Confidence Interval for the Variance, ci.var() -----------------------------
  }, ci.var = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No grouping ####

    if (isTRUE(is.null(x$data$group) && is.null(x$data$split))) {

      #—————————————————————————————————————— #
      ### Round ####

      write.object[, !sapply(write.object, is.character)] <- sapply(write.object[, !sapply(write.object, is.character)], round, digits = digits)

      #—————————————————————————————————————— #
      ### Column Names ####

      colnames(write.object) <- c("Variable", "n", "nNA", "pNA", "Skew", "Kurt", "M", "Var", "Low", "Upp")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Grouping ####

    } else if (isTRUE(!is.null(x$data$group) && is.null(x$data$split))) {

      #—————————————————————————————————————— #
      ### Round ####

      write.object[, !sapply(write.object, is.character)] <- sapply(write.object[, !sapply(write.object, is.character)], round, digits = digits)

      #—————————————————————————————————————— #
      ### Remove Duplicated Values ####

      write.object[duplicated(write.object$group) , "group"] <- ""

      #—————————————————————————————————————— #
      ### Column Names ####

      colnames(write.object) <- c("Group", "Variable", "n", "nNA", "pNA", "Skew", "Kurt", "M", "Var", "Low", "Upp")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Split ####

    } else if (isTRUE(!is.null(x$data$split))) {

      #—————————————————————————————————————— #
      ### No Grouping ####

      if (isTRUE(is.null(x$data$group))) {

        for (i in names(write.object)) {

          #### Round ####
          write.object[[i]][, !sapply(write.object[[i]], is.character)] <- sapply(write.object[[i]][, !sapply(write.object[[i]], is.character)], round, digits = digits)

          #### Column Names ####
          colnames(write.object[[i]]) <- c("Variable", "n", "nNA", "pNA", "Skew", "Kurt", "M", "Var", "Low", "Upp")

        }

      #—————————————————————————————————————— #
      ### Grouping ####

      } else {

        for (i in names(write.object)) {

          #### Round ####
          write.object[[i]][, !sapply(write.object[[i]], is.character)] <- sapply(write.object[[i]][, !sapply(write.object[[i]], is.character)], round, digits = digits)

          #### Remove Duplicated Values ####
          write.object[[i]][duplicated(write.object[[i]]$group) , "group"] <- ""

          #### Column Names ####
          colnames(write.object[[i]]) <- c("Group", "Variable", "n", "nNA", "pNA", "Skew", "Kurt", "M", "Var", "Low", "Upp")

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Note Bootstrapping ####

    if (isTRUE(x$args$boot != "none")) {

      note <- data.frame(c("Alternative Hypothesis:", "Confidence Level:", "Bootstrap Method:", "Replications:"),
                         c(x$args$alternative, x$args$conf.level,
                           switch(x$args$boot,
                                  "perc" = "Percentile bootstrap CI",
                                  "bc" = "Bias-corrected (BC) percentile bootstrap CI",
                                  "bca" = "Bias-corrected and accelerated (BCa) bootstrap CI"),
                           x$args$R),
                         fix.empty.names = FALSE)


      #—————————————————————————————————————— #
      ### Write Object ####

      if (isTRUE(is.data.frame(write.object))) {

        write.object <- list("CI Var" = write.object, Note = note)

      } else {

        write.object <- append(write.object, list(Note = note))

      }

    } else {

      if (isTRUE(is.data.frame(write.object))) {

        write.object <- list("CI Var" = write.object)

      }

    }

  #_____________________________________________________________________________
  #
  # Confidence Interval for the Standard Deviation, ci.sd() --------------------
  }, ci.sd = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No grouping ####

    if (isTRUE(is.null(x$data$group) && is.null(x$data$split))) {

      #—————————————————————————————————————— #
      ### Round ####

      write.object[, !sapply(write.object, is.character)] <- sapply(write.object[, !sapply(write.object, is.character)], round, digits = digits)

      #—————————————————————————————————————— #
      ### Column Names ####

      colnames(write.object) <- c("Variable", "n", "nNA", "pNA", "Skew", "Kurt", "M", "SD", "Low", "Upp")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Grouping ####

    } else if (isTRUE(!is.null(x$data$group) && is.null(x$data$split))) {

      #—————————————————————————————————————— #
      ### Round ####

      write.object[, !sapply(write.object, is.character)] <- sapply(write.object[, !sapply(write.object, is.character)], round, digits = digits)

      #—————————————————————————————————————— #
      ### Remove Duplicated Values ####

      write.object[duplicated(write.object$group) , "group"] <- ""

      #—————————————————————————————————————— #
      ### Column Names ####

      colnames(write.object) <- c("Group", "Variable", "n", "nNA", "pNA", "Skew", "Kurt", "M", "SD", "Low", "Upp")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Split ####

    } else if (isTRUE(!is.null(x$data$split))) {

      #—————————————————————————————————————— #
      ### No Grouping ####

      if (isTRUE(is.null(x$data$group))) {

        for (i in names(write.object)) {

          ### Round ####
          write.object[[i]][, !sapply(write.object[[i]], is.character)] <- sapply(write.object[[i]][, !sapply(write.object[[i]], is.character)], round, digits = digits)

          ### Column Names ####
          colnames(write.object[[i]]) <- c("Variable", "n", "nNA", "pNA", "Skew", "Kurt", "M", "SD", "Low", "Upp")

        }

      #—————————————————————————————————————— #
      ### Grouping ####

      } else {

        for (i in names(write.object)) {

          ### Round ####
          write.object[[i]][, !sapply(write.object[[i]], is.character)] <- sapply(write.object[[i]][, !sapply(write.object[[i]], is.character)], round, digits = digits)

          ### Remove Duplicated Values ####
          write.object[[i]][duplicated(write.object[[i]]$group) , "group"] <- ""

          ### Column Names ####
          colnames(write.object[[i]]) <- c("Group", "Variable", "n", "nNA", "pNA", "Skew", "Kurt", "M", "SD", "Low", "Upp")

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Note Bootstrapping ####

    if (isTRUE(x$args$boot != "none")) {

      note <- data.frame(c("Alternative Hypothesis:", "Confidence Level:", "Bootstrap Method:", "Replications:"),
                         c(x$args$alternative, x$args$conf.level,
                           switch(x$args$boot,
                                  "perc" = "Percentile bootstrap CI",
                                  "bc" = "Bias-corrected (BC) percentile bootstrap CI",
                                  "bca" = "Bias-corrected and accelerated (BCa) bootstrap CI"),
                           x$args$R),
                         fix.empty.names = FALSE)

      #—————————————————————————————————————— #
      ### Write Object ####

      if (isTRUE(is.data.frame(write.object))) {

        write.object <- list("CI Sd" = write.object, Note = note)

      } else {

        write.object <- append(write.object, list(Note = note))

      }

    } else {

      if (isTRUE(is.data.frame(write.object))) {

        write.object <- list("CI SD" = write.object)

      }

    }

  #_____________________________________________________________________________
  #
  # Correlation Matrix, cor.matrix() -------------------------------------------
  }, cor.matrix = {

    # Round
    write.object$cor <- round(write.object$cor, digits = digits)

    if (isTRUE(!x$args$method %in% c("tetra", "poly"))) {

      write.object$stat <- round(write.object$stat, digits = digits)
      write.object$p <- round(write.object$p, digits = p.digits)

    }

    # Diagonal
    diag(write.object$cor) <- NA
    diag(write.object$n) <- NA

    if (isTRUE(!x$args$method %in% c("tetra", "poly"))) {

      diag(write.object$stat) <- NA
      diag(write.object$df) <- NA
      diag(write.object$p) <- NA

    }

    # Lower and/or upper triangular
    if (isTRUE(!".group" %in% colnames(x$data))) {

      if (isTRUE(x$args$tri == "lower")) {

        write.object$cor[upper.tri(write.object$cor)] <- NA
        write.object$n[upper.tri(write.object$n)] <- NA

        if (isTRUE(!x$args$method %in% c("tetra", "poly"))) {

          write.object$stat[upper.tri(write.object$stat)] <- NA
          write.object$df[upper.tri(write.object$df)] <- NA
          write.object$p[upper.tri(write.object$p)] <- NA

        }

      }

      if (isTRUE(x$args$tri == "upper")) {

        write.object$cor[lower.tri(write.object$cor)] <- NA
        write.object$n[lower.tri(write.object$n)] <- NA

        if (isTRUE(!x$args$method %in% c("tetra", "poly"))) {

          write.object$stat[lower.tri(write.object$stat)] <- NA
          write.object$df[lower.tri(write.object$df)] <- NA
          write.object$p[lower.tri(write.object$p)] <- NA

        }

      }

    }

    # Add variable names in the rows
    write.object <- lapply(write.object, function(y) data.frame(colnames(y), y,
                                                                row.names = NULL, check.rows = FALSE,
                                                                check.names = FALSE, fix.empty.names = FALSE))

    # Add infos
    write.object$Info <- data.frame(c("Correlation coefficient:", "Missing data:", "Adjustment for multiple testing:"),
                                    c(switch(x$args$method, "pearson" = "Pearson Product-Moment",
                                                            "spearman" = "Spearman's Rank-Order",
                                                            "kendall-b" = "Kendall's Tau-b",
                                                            "kendall-c" = "Kendall-Stuart's Tau-c",
                                                            "tetra" = "Tetrachoric",
                                                            "poly" = "Polychoric"),
                                      ifelse(isTRUE(attr(x$data, "missing")), ifelse(isTRUE(x$args$na.omit), "Listwise deletion", "Pairwise deletion"), "No missing data"),
                                      ifelse(x$args$p.adj == "none", "None", x$args$p.adj)),
                                      row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

    if (isTRUE(x$args$method %in% c("tetra", "poly"))) { write.object$Info <- write.object$Info[-3L, ] }

    # Grouping
    if (isTRUE(".group" %in% colnames(x$data))) { write.object$Info <- rbind(write.object$Info, c(paste0("Lower triangular: ", sort(unique(x$data$.group))[1L], ", Upper triangular: ", sort(unique(x$data$.group))[2L]), NA)) }

    if (isTRUE(!x$args$method %in% c("tetra", "poly"))) {

      names(write.object) <- c("Cor", "n", "Stat", "df", "p", "Info")

    } else {

      names(write.object) <- c("Cor", "n", "Info")

    }

    # Print
    if (isTRUE(!"cor" %in% write)) { write.object$Cor <- NULL }
    if (isTRUE(!"n" %in% write)) { write.object$n <- NULL }

    if (isTRUE(!x$args$method %in% c("tetra", "poly"))) {

      if (isTRUE(!"stat" %in% write)) { write.object$Stat <- NULL }
      if (isTRUE(!"df" %in% write)) { write.object$df <- NULL }
      if (isTRUE(!"p" %in% write)) { write.object$p <- NULL }

    }

  #_____________________________________________________________________________
  #
  # HC and CR Stadard Errors, coeff.robust() -----------------------------------
  }, coeff.robust = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Model Class ####

    # (Generalized) Linear Model
    if (isTRUE(any(class(x$model) == "lm"))) {

      model.class <- "lm"

    # Multilevel and Linear Mixed-Effects Model
    } else if (all(class(x$model) %in% c("lmerMod", "lmerModLmerTest"))) {

      model.class <- "lmer"

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Choose Model Class ####

    switch(model.class,

           #—————————————————————————————————————— #
           ### Linear Regression, lm() ####

           lm = {

             #···················
             #### Coefficient Result Table ####

             write.coef <- write.object$coef

             # Round
             write.coef[, setdiff(colnames(write.coef), "p")] <- sapply(write.coef[, setdiff(colnames(write.coef), "p")], round, digits = digits)
             write.coef[, "p"] <- round(write.coef[, "p"], digits = p.digits)

             # Row names
             write.coef <- data.frame(row.names(write.coef), write.coef, check.names = FALSE, fix.empty.names = FALSE)

             #···················
             #### F-test Result Table ####

             write.F <- NULL
             if (isTRUE(!is.null(write.object$F.test))) {

               write.F <- write.object$F.test

               write.F[, 3L] <- sapply(write.F[, 3L], round, digits = digits)
               write.F[, 4L] <- round(write.F[, 4L], digits = p.digits)

             }

             #···················
             #### Sandwich Result Table ####

             write.sandwich <-round(as.data.frame(as.matrix(write.object$sandwich)), digits = digits)

             # Row names
             if (isTRUE(x$args$type %in% c("HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5"))) {

               write.sandwich <- data.frame(row.names(write.sandwich), write.sandwich, check.names = FALSE, fix.empty.names = FALSE)

             }

             #···················
             #### Write Object ####

             if (isTRUE(!is.null(write.F))) {

               write.object <- list(Coef = write.coef, F.test = write.F, Sandwich = write.sandwich)

             } else {

               write.object <- list(Coef = write.coef, Sandwich = write.sandwich)

             }

           #—————————————————————————————————————— #
           ### Linear Mixed-Effects Model, lmer() ####

           }, lmer = {

             #...................
             #### Extract coefficients ####

             write.coef <- write.object$coef

             #...................
             #### Round ####

             write.coef[, setdiff(colnames(write.coef), "p")] <- sapply(write.coef[, setdiff(colnames(write.coef), "p")], round, digits = digits)

             if (isTRUE("p" %in% colnames(write.coef))) { write.coef[, "p"] <- round(write.coef[, 4L], digits = p.digits) }

             #...................
             #### Sandwich result table ####

             write.sandwich <- round(as.data.frame(as.matrix(write.object$sandwich)), digits = digits)

             # Row names
             write.sandwich <- data.frame(row.names(write.sandwich), write.sandwich, check.names = FALSE, fix.empty.names = FALSE)

             #...................
             #### Write object ####

             write.object <- list(Coef = write.coef, Sandwich = write.sandwich)

           })

  #_____________________________________________________________________________
  #
  # Standardized Coefficients --------------------------------------------------
  }, coeff.std = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Round ####

    #—————————————————————————————————————— #
    ### Linear Model, lm() Function ####

    if (isTRUE(class(x$model) == "lm")) {

      write.object[, -4L] <- apply(write.object[, -4L], 2L, round, digits)
      write.object[, 4L] <- round(write.object[, 4L], digits = p.digits)

    #—————————————————————————————————————— #
    ### Linear Mixed-Effects Model, lmer() function ####

    } else if (isTRUE(class(x$model) %in% c("lmerMod", "lmerModLmerTest"))) {

      write.object[, !colnames(write.object) %in% c("p", "Level")] <- apply(write.object[, !colnames(write.object) %in% c("p", "Level")], 2L, round, digits)

      if (isTRUE("p)" %in% colnames(write.object))) { write.object[, colnames(write.object) == "p"] <- round(write.object[, colnames(write.object) == "p"], digits = p.digits) }

    #—————————————————————————————————————— #
    ### Linear Mixed-Effects Model, lme() function ####

    } else if (isTRUE(class(x$model) == "lme")) {

      write.object[, !colnames(write.object) %in% c("p", "Level")] <- apply(write.object[, !colnames(write.object) %in% c("p", "Level")], 2L, round, digits)

      if (isTRUE("p" %in% colnames(write.object))) { write.object[, colnames(write.object) == "p"] <- round(write.object[, colnames(write.object) == "p"], digits = p.digits) }

    }

    # Row names
    write.coef <- data.frame(row.names(write.object), write.object, fix.empty.names = FALSE, check.names = FALSE)

    #—————————————————————————————————————— #
    ### Write Object ####

    write.object <- list(Coef = write.coef)

  #_____________________________________________________________________________
  #
  # Cross Tabulation, crosstab() -----------------------------------------------

  }, crosstab = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Result table ####

    write.object <- x$result$crosstab

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Round ####

    write.object[, !sapply(write.object, is.character)] <- sapply(write.object[, !sapply(write.object, is.character)], round, digits = digits)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Two-Dimensional Matrix ####

    if (isTRUE(ncol(x$data) == 2L)) {

      #—————————————————————————————————————— #
      ### Output Table Not Split ####

      if (!isTRUE(x$args$split)) {

        # Remove duplicated row labels
        write.object[, 1L] <- ifelse(duplicated(write.object[, 1L]), NA, write.object[, 1L])

        #### Frequencies only ####
        if (isTRUE(write == "no")) {

          write.object <- data.frame(write.object[write.object[, 2L] == "Freq" | is.na(write.object[, 2L]) , 1L],
                                     write.object[write.object[, 2L] == "Freq" | is.na(write.object[, 2L]), -c(1L, 2L)],
                                     row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

        #### Frequencies and Percentages ####
        } else {

          # No row-wise percentages
          if (isTRUE(!"row" %in% write)) { write.object <- write.object[-which(write.object[, 2L] == "Row %"), ] }

          # No col-wise percentages
          if (isTRUE(!"col" %in% write)) { write.object <- write.object[-which(write.object[, 2L] == "Col %"), ] }

          # No total percentages
          if (isTRUE(!"total" %in% write)) { write.object <- write.object[-which(write.object[, 2L] == "Tot %"), ] }

        }

        # Add variable names
        names(write.object)[1L:2L] <- colnames(x$data)

      #—————————————————————————————————————— #
      ### Output Table Split ####

      } else {

        #### Absolute Frequencies ####
        write.object.abs <- data.frame(write.object[write.object[, 2L] == "Freq" | is.na(write.object[, 2L]), 1L],
                                       write.object[write.object[, 2L] == "Freq" | is.na(write.object[, 2L]), -c(1L, 2L)],
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        write.object.abs <- data.frame(c(colnames(x$data)[1L], rep(NA, times = nrow(write.object.abs) - 1L)),
                                       write.object.abs,
                                       row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

        names(write.object.abs)[2L] <- colnames(x$data)[2L]

        #### Row-wise percentages ####
        write.object.row <- data.frame(write.object[which(write.object[, 2L] == "Row %"), 1L],
                                       write.object[which(write.object[, 2L] == "Row %"), -c(1L, 2L)],
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        write.object.row <- data.frame(c(colnames(x$data)[1L], rep(NA, times = nrow(write.object.row) - 1L)),
                                       write.object.row,
                                       row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

        names(write.object.row)[2L] <- colnames(x$data)[2L]

        #### Column-wise percentages ####
        write.object.col <- data.frame(write.object[which(write.object[, 2L] == "Col %"), 1L],
                                       write.object[which(write.object[, 2L] == "Col %"), -c(1L, 2L)],
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        write.object.col <- data.frame(c(colnames(x$data)[1L], rep(NA, times = nrow(write.object.col) - 1L)),
                                       write.object.col,
                                       row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

        names(write.object.col)[2L] <- colnames(x$data)[2L]

        #### Total percentages ####
        write.object.tot <- data.frame(write.object[write.object[, 2L] == "Tot %", 1L],
                                       write.object[write.object[, 2L] == "Tot %", -c(1L, 2L)],
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        write.object.tot <- data.frame(c(colnames(x$data)[1L], rep(NA, times = nrow(write.object.tot) - 1L)),
                                       write.object.tot,
                                       row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

        names(write.object.tot)[2L] <- colnames(x$data)[2L]

        #### Prepare list ####
        write.object <- list()

        if (isTRUE(x$args$freq)) { write.object$"Freq" <- write.object.abs }

        if (isTRUE("row" %in% write)) { write.object$"Row%" <- write.object.row }

        if (isTRUE("col" %in% write)) { write.object$"Col%" <- write.object.col }

        if (isTRUE("total" %in% write)) { write.object$"Total%" <- write.object.tot }

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Three-Dimensional Matrix ####

    } else if (isTRUE(ncol(x$data) == 3L)) {

      #—————————————————————————————————————— #
      ### Output Table Not Split ####

      if (!isTRUE(x$args$split)) {

        # Remove duplicated row labels
        duplic <- apply(write.object[, c(1L:2L)], 1L, paste, collapse = "")

        write.object[, 1L] <- ifelse(duplicated(duplic), NA, write.object[, 1L])
        write.object[, 2L] <- ifelse(duplicated(duplic), NA, write.object[, 2L])

        write.object[, 1L] <- ifelse(duplicated(write.object[, 1L]), NA, write.object[, 1L])

        #### Frequencies only ####
        if (isTRUE(write == "no")) {

          write.object <- data.frame(write.object[write.object[, 3L] == "Freq" | is.na(write.object[, 3L]), 1L],
                                     write.object[write.object[, 3L] == "Freq" | is.na(write.object[, 3L]), -c(1L, 3L)],
                                     row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

          # Add variable names
          write.object <- data.frame(c(colnames(x$data)[1L], rep(NA, times = nrow(write.object) - 1L)),
                                     write.object,
                                     row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

          names(write.object)[c(2L, 3L)] <- colnames(x$data)[c(2L, 3L)]

        #### Frequencies and Percentages ####
        } else {

          # No row-wise percentages
          if (isTRUE(!"row" %in% write)) { write.object <- write.object[-which(write.object[, 3L] == "Row %"), ] }

          # No col-wise percentages
          if (isTRUE(!"col" %in% write)) { write.object <- write.object[-which(write.object[, 3L] == "Col %"), ] }

          # No total percentages
          if (isTRUE(!"total" %in% write)) { write.object <- write.object[-which(write.object[, 3L] == "Tot %"), ] }

          # Add variable names
          names(write.object)[c(1L, 2L, 3L)] <- colnames(x$data)

        }

      #—————————————————————————————————————— #
      ### Output Table Split ####

      } else {

        #### Absolute Frequencies ####
        write.object.abs <- data.frame(write.object[write.object[, 3L] == "Freq" | is.na(write.object[, 3L]), 1L],
                                       write.object[write.object[, 3L] == "Freq" | is.na(write.object[, 3L]), -c(1L, 3L)],
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        # Remove duplicated row labels
        write.object.abs[, 1L] <- ifelse(duplicated(write.object.abs[, 1L]), NA, write.object.abs[, 1L])

        # Add variable names
        write.object.abs <- data.frame(c(colnames(x$data)[1L], rep(NA, times = nrow(write.object.abs) - 1L)),
                                       write.object.abs,
                                       row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

        names(write.object.abs)[c(2L, 3L)] <- colnames(x$data)[c(2L, 3L)]

        #### Row-wise percentages ####
        write.object.row <- data.frame(write.object[which(write.object[, 3L] == "Row %"), 1L],
                                       write.object[which(write.object[, 3L] == "Row %"), -c(1L, 3L)],
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        # Remove duplicated row labels
        write.object.row[, 1L] <- ifelse(duplicated(write.object.row[, 1L]), NA, write.object.row[, 1L])

        # Add variable names
        write.object.row <- data.frame(c(colnames(x$data)[1L], rep(NA, times = nrow(write.object.row) - 1L)),
                                       write.object.row,
                                       row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

        names(write.object.row)[c(2L, 3L)] <- colnames(x$data)[c(2L, 3L)]


        #### Column-wise percentages ####
        write.object.col <- data.frame(write.object[which(write.object[, 3L] == "Col %"), 1L],
                                       write.object[which(write.object[, 3L] == "Col %"), -c(1L, 3L)],
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        # Remove duplicated row labels
        write.object.col[, 1L] <- ifelse(duplicated(write.object.col[, 1L]), NA, write.object.col[, 1L])

        # Add variable names
        write.object.col <- data.frame(c(colnames(x$data)[1L], rep(NA, times = nrow(write.object.col) - 1L)),
                                       write.object.col,
                                       row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

        names(write.object.col)[c(2L, 3L)] <- colnames(x$data)[c(2L, 3L)]

        #### Total percentages ####
        write.object.tot <- data.frame(write.object[write.object[, 3L] == "Tot %", 1L],
                                       write.object[write.object[, 3L] == "Tot %", -c(1L, 3L)],
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        # Remove duplicated row labels
        write.object.tot[, 1L] <- ifelse(duplicated(write.object.tot[, 1L]), NA, write.object.tot[, 1L])

        # Add variable write.object.tot
        write.object.tot <- data.frame(c(colnames(x$data)[1L], rep(NA, times = nrow(write.object.col) - 1L)),
                                       write.object.tot,
                                       row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

        names(write.object.tot)[c(2L, 3L)] <- colnames(x$data)[c(2L, 3L)]

        #### Prepare list ####
        write.object <- list()

        if (isTRUE(x$args$freq)) { write.object$"Freq" <- write.object.abs }

        if (isTRUE("row" %in% write)) { write.object$"Row%" <- write.object.row }

        if (isTRUE("col" %in% write)) { write.object$"Col%" <- write.object.col }

        if (isTRUE("total" %in% write)) { write.object$"Total%" <- write.object.tot }

      }

    }
  #_____________________________________________________________________________
  #
  # Descriptive Statistics, descript() -----------------------------------------

  }, descript = {

    # Variables to round
    write.round <- c("pNA", "m", "se.m", "var", "sd", "min", "p.min", "p25", "med", "p75", "max", "p.max", "range", "iqr", "skew", "kurt")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No Grouping, No Split ####

    if (isTRUE(is.null(x$data$group) && is.null(x$data$split))) {

      # Round
      write.object[, write.round] <- sapply(write.round, function(y) ifelse(!is.na(write.object[, y]), round(write.object[, y], digits = digits), NA))

      # Select statistical measures
      print <- match(write, names(write.object))

      # Variable names
      names(write.object) <- c("Variable", "n", "nNA", "%NA", "nUQ", "M", "SE.M", "Var", "SD", "Min", "%Min", "p25", "Med", "p75", "Max", "%Max", "Range", "IQR", "Skew", "Kurt")

      # One variable
      if (isTRUE(ncol(x$data$x) == 1L)) {

        # Select statistical measures
        write.object <- write.object[, print]

      # More than one variable
      } else {

        # Select statistical measures
        write.object <- write.object[, c(1L, print)]

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Grouping, No Split ####

    } else if (isTRUE(!is.null(x$data$group) && is.null(x$data$split))) {

      # Round
      write.object[, write.round] <- sapply(write.round, function(y) ifelse(!is.na(write.object[, y]), round(write.object[, y], digits = digits), NA))

      # Select statistical measures
      print <- match(write, names(write.object))

      # Variable names
      names(write.object) <- c("Group", "Variable", "n", "nNA", "%NA", "nUQ", "M", "SE.M", "Var", "SD", "Min", "%Min", "p25", "Med", "p75", "Max", "%Max", "Range", "IQR", "Skew", "Kurt")

      # One variable
      if (isTRUE(ncol(x$data$x) == 1L)) {

        # Select statistical measures
        write.object <- write.object[, c(1L, print)]

      # More than one variable
      } else {

        # Select statistical measures
        write.object <- write.object[, c(1L, 2L, print)]

      }

      # Convert to numeric
      write.object$Group <- ifelse(grepl("(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))$)|(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))e(-|\\+)?(\\d+)$)",
                                         x = write.object$Group), as.numeric(write.object$Group), write.object$Group)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Split, without or With Grouping ####

    } else if (isTRUE(!is.null(x$data$split))) {

      # Round
      for (i in names(write.object)) { write.object[[i]][, write.round] <- sapply(write.round, function(y) ifelse(!is.na(write.object[[i]][, y]), round(write.object[[i]][, y], digits = digits), NA)) }

      #—————————————————————————————————————— #
      ### No Grouping ####

      if (isTRUE(is.null(x$data$group))) {

        # Select statistical measures
        print <- match(write, names(write.object[[1]]))

        # Variable names
        write.object <- lapply(write.object, function(y) misty::df.rename(y, from = names(y), to = c("Variable", "n", "nNA", "%NA", "nUQ", "M", "SE.M", "Var", "SD", "Min", "%Min", "p25", "Med", "p75", "Max", "%Max", "Range", "IQR", "Skew", "Kurt")))

        # One variable
        if (isTRUE(ncol(x$data$x) == 1L)) {

          # Select statistical measures
          write.object <- lapply(write.object, function(y) y[, ])

        # More than one variable
        } else {

          # Select statistical measures
          write.object <- lapply(write.object, function(y) y[, c(1L, print)])

        }

      #—————————————————————————————————————— #
      ### Grouping ####

      } else {

        # Select statistical measures
        print <- match(write, names(write.object[[1]]))

        # Variable names
        write.object <- lapply(write.object, function(y) misty::df.rename(y, from = names(y), to = c("Group", "Variable", "n", "nNA", "%NA", "M", "SE.M", "Var", "SD", "Min", "%Min", "p25", "Med", "p75", "Max", "%Max", "Range", "IQR", "Skew", "Kurt")))

        # One variable
        if (isTRUE(ncol(x$data$x) == 1L)) {

          # Select statistical measures
          write.object <- lapply(write.object, function(y) y[, c(1L, print)])

        # More than one variable
        } else {

          # Select statistical measures
          write.object <- lapply(write.object, function(y) y[, c(1L, 2L, print)])

        }

        # Convert to numeric
        write.object <- lapply(write.object, function(y) within(y, assign("Group", ifelse(grepl("(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))$)|(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))e(-|\\+)?(\\d+)$)",
                                                                          x = y$Group), as.numeric(y$Group), y$Group))))

      }

    }

  #_____________________________________________________________________________
  #
  # Dominance Analysis, Manual, dominance.manual() -----------------------------

  }, dominance.manual = {

    # Extract result table
    write.gen <- write.object

    #—————————————————————————————————————— #
    ### Round ####

    write.gen[, "r2"] <- round(write.gen[, "r2"], digits = digits)
    write.gen[, "perc"] <- round(write.gen[, "perc"], digits = digits - 1L)

    #—————————————————————————————————————— #
    ### Variable Names ####

    write.gen <- data.frame(Variable = rownames(write.gen), write.gen)

    #—————————————————————————————————————— #
    ### Write Object ####

    write.object <- list(General = write.gen)

  #_____________________________________________________________________________
  #
  # Chi-Bar-Square Difference Test, difftest.chibarsq() ------------------------
  }, difftest.chibarsq = {

    # Write object
    write.object <- write.object$difftest

    # Remove chisq.crit column
    write.object <- write.object[, setdiff(colnames(write.object), "chisq.crit")]

    # Round variables
    write.object[, setdiff(colnames(write.object), c("df", "df.diff", "p"))] <- sapply(setdiff(colnames(write.object), c("df", "df.diff", "p")), function(y) round(write.object[, y], digits = digits))
    write.object[, "p"] <- round(write.object[, "p"], digits = p.digits)

    # Column
    colnames(write.object) <- c("df", "AIC", "BIC", "SABIC", "Chisq", "dChisq", "ddf","p")

    # Rows
    write.object <- data.frame(row.names(write.object), write.object, fix.empty.names = FALSE)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Write Object ####

    write.object <- list(Difftest = summary, Weights = round(x$result$weights, digits = digits))

  #_____________________________________________________________________________
  #
  # Dominance Analysis, dominance() --------------------------------------------

  }, dominance = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## General Dominance ####

    print.gen <- NULL
    if (isTRUE("gen" %in% write)) {

      # Extract result table
      write.gen <- write.object$gen

      #—————————————————————————————————————— #
      ### Round ####

      write.gen[, "r2"] <- round(write.gen[, "r2"], digits = digits)
      write.gen[, "perc"] <- round(write.gen[, "perc"], digits = digits - 1L)

      #—————————————————————————————————————— #
      ### Variable Names ####

      write.gen <- data.frame(Variable = rownames(write.gen), write.gen)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Conditional Dominance ####

    write.cond <- NULL
    if (isTRUE("cond" %in% write)) {

      # Extract result table
      write.cond <- write.object$cond

      #—————————————————————————————————————— #
      ### Variable Names ####

      write.cond <- data.frame(Variable = rownames(write.cond), write.cond)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Complete Dominance ####

    write.comp <- NULL
    if (isTRUE("cond" %in% write)) {

      # Extract result table
      write.comp <- write.object$comp

      #—————————————————————————————————————— #
      ### Variable Names ####

      write.comp <- data.frame(Variable = rownames(write.comp), write.comp)

    }

    #...................
    ### Write object ####

    write.object <- list(General = write.gen, Conditional = write.cond, Complete = write.comp)

    write.object <- write.object[unlist(lapply(write.object, function(y) !is.null(y)))]

  #_____________________________________________________________________________
  #
  # Effect Sizes for Categorical Variables, effsize() --------------------------

  }, effsize = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Round ####

    write.object[, colnames(write.object)[!colnames(write.object) %in% c("n", "var")]] <- round(write.object[, colnames(write.object)[!colnames(write.object) %in% c("n", "var")]], digits = digits)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Label ####

    note <- paste0(switch(x$args$type,
                   phi = {

                      if (isTRUE(x$args$adjust)) { "Adjusted Phi Coefficient: " } else { "Phi Coefficient: " }

                   }, cramer = {

                      if (isTRUE(x$args$adjust)) { "Bias-Corrected Cramer's V: " } else { "Cramer's V: " }

                    }, tschuprow = {

                      if (isTRUE(x$args$adjust)) { "Bias-Corrected Tschuprow's T: " } else { "Tschuprow's T: " }

                    }, cont = {

                      if (isTRUE(x$args$adjust)) { "Adjusted Pearson's Contingency Coefficient: " } else { "Pearson's Contingency Coefficient: " }

                    }, w = { cat(" Cohen's w: ")
                    }, fei = { " Fei: "}),
               switch(x$args$alternative,
                      two.sided = "Two-Sided ",
                      less = "One-Sided ",
                      greater = "One-Sided "),
               paste0(round(x$args$conf.level * 100L, digits = 2L), "% "), "Confidence Interval")

    if (isTRUE(x$args$indep && ncol(x$data) > 2L)) { note <- c(note, paste0("The focal variable is ", colnames(x$data)[1L])) }

    write.object <- list(Effsize = write.object, Note = data.frame(Note = note, row.names = NULL))

  #_____________________________________________________________________________
  #
  # Frequency Table, freq() ----------------------------------------------------

  }, freq = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## One Variable ####

    if (isTRUE(ncol(x$data) == 1L)) {

      #—————————————————————————————————————— #
      ### Values Shown in Columns, Variables in the Rows ####

      if (isTRUE(x$args$val.col)) {

        # Complete data
        if (isTRUE(all(!is.na(x$data)))) {

          write.object <- data.frame(Value = c("Freq", "Perc"),
                                     write.object[-nrow(write.object), -ncol(write.object)],
                                     Total = rowSums(write.object[-nrow(write.object), -ncol(write.object)]),
                                     Missing = write.object[-nrow(write.object), ncol(write.object)],
                                     fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

        # Missing data
        } else {

          write.object <- data.frame(Value = c("Freq", "Perc", "Valid Perc"),
                                     write.object[, -ncol(write.object)],
                                     Total = rowSums(write.object[, -ncol(write.object)]),
                                     Missing = write.object[, ncol(write.object)],
                                     Total = rowSums(write.object),
                                     fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

        }

      #—————————————————————————————————————— #
      ### Values Shown in Rows, Variables in the Columns ####

      } else {

        # Complete data
        if (isTRUE(all(!is.na(x$data)))) {

          write.object <- data.frame(c("Value", rep("", times = nrow(write.object) - 2L), "Total", "Missing"),
                                     c(write.object[, "Value"], NA),
                                     Freq = c(write.object[1:nrow(write.object) - 1L, "Freq"],
                                              sum(write.object[1:nrow(write.object) - 1L, "Freq"]),
                                              write.object[nrow(write.object), "Freq"]),
                                     Perc = c(write.object[1:nrow(write.object) - 1L, "Perc"],
                                              sum(write.object[1:nrow(write.object) - 1L, "Perc"]),
                                              write.object[nrow(write.object), "Perc"]),
                                     fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

          colnames(write.object) <- c("", "", "Freq", "Perc")

        # Missing data
        } else {

          write.object <- data.frame(c("Value", rep("", times = nrow(write.object) - 2L), "Total", "Missing", "Total"),
                                     c(write.object[, "Value"], NA, NA),
                                     Freq = c(write.object[1:nrow(write.object) - 1L, "Freq"],
                                              sum(write.object[1:nrow(write.object) - 1L, "Freq"]),
                                              write.object[nrow(write.object), "Freq"],
                                              sum(write.object[, "Freq"])),
                                     Perc = c(write.object[1:nrow(write.object) - 1L, "Perc"],
                                              sum(write.object[1:nrow(write.object) - 1L, "Perc"]),
                                              write.object[nrow(write.object), "Perc"],
                                              sum(write.object[, "Perc"])),
                                     V.Perc = c(write.object[1:nrow(write.object) - 1L, "V.Perc"],
                                                sum(write.object[1:nrow(write.object) - 1L, "V.Perc"]), NA, NA),
                                     fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

          colnames(write.object) <- c("", "", "Freq", "Perc", "Valid Perc")

        }

      }

      # Round digits
      write.object[, !sapply(write.object, is.character)] <- sapply(write.object[, !sapply(write.object, is.character)], round, digits = digits)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## More Than One Variable ####

    } else {

      #—————————————————————————————————————— #
      ### Variables Split to Multiple Excel Sheets ####

      if (isTRUE(x$args$split)) {

        write.object <- lapply(write.object, function(y) {

          #···················
          #### Values Shown in Columns, Variables in the Rows ####

          if (isTRUE(x$args$val.col)) {

            # Complete data
            if (isTRUE(y[1L, ncol(y)] == 0L)) {

              data.frame(Value = c("Freq", "Perc"),
                         y[-nrow(y), -ncol(y)], Total = rowSums(y[-nrow(y), -ncol(y)]),
                         Missing = y[-nrow(y), ncol(y)],
                         fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            } else {

              data.frame(Value = c("Freq", "Perc", "Valid Perc"),
                         y[, -ncol(y)],
                         Total = rowSums(y[, -ncol(y)]),
                         Missing = y[, ncol(y)],
                         Total = rowSums(y),
                         fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            }

          #···················
          #### Values Shown in Rows, Variables in the Columns ####

          } else {

            # Complete data
            if (isTRUE(y[nrow(y), "Freq"] == 0L)) {

              data.frame(c("Value", rep("", times = nrow(y) - 2L), "Total", "Missing"),
                         c(y[, "Value"], NA),
                         Freq = c(y[1:nrow(y) - 1L, "Freq"], sum(y[1:nrow(y) - 1L, "Freq"]), y[nrow(y), "Freq"]),
                         Perc = c(y[1:nrow(y) - 1L, "Perc"], sum(y[1:nrow(y) - 1L, "Perc"]), y[nrow(y), "Perc"]),
                         fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            } else {

              data.frame(c("Value", rep("", times = nrow(y) - 2L), "Total", "Missing", "Total"),
                         c(y[, "Value"], NA, NA),
                         Freq = c(y[1:nrow(y) - 1L, "Freq"], sum(y[1:nrow(y) - 1L, "Freq"]),
                                  y[nrow(y), "Freq"],
                                  sum(y[, "Freq"])),
                         Perc = c(y[1:nrow(y) - 1L, "Perc"], sum(y[1:nrow(y) - 1L, "Perc"]), y[nrow(y), "Perc"], sum(y[, "Perc"])),
                         V.Perc = c(y[1:nrow(y) - 1L, "V.Perc"], sum(y[1:nrow(y) - 1L, "V.Perc"]), NA, NA),
                         fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            }

          }

        })

      #—————————————————————————————————————— #
      ### Variables not Split to Multiple Excel Sheets ####

      } else {

        #···················
        #### Values Shown in Columns, Variables in the Rows ####

        if (isTRUE(x$args$val.col)) {

          # Complete data
          if (isTRUE(all(!is.na(x$data)))) {

            write.object$freq <- data.frame(write.object$freq[, "Var"],
                                            write.object$freq[, -c(1L, ncol(write.object$freq))],
                                            Total = rowSums(write.object$freq[, -c(1L, ncol(write.object$freq))]),
                                            Missing = write.object$freq[, ncol(write.object$freq)],
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$perc <- data.frame(write.object$perc[, "Var"],
                                            write.object$perc[, -c(1L, ncol(write.object$perc))],
                                            Total = rowSums(write.object$perc[, -c(1L, ncol(write.object$perc))]),
                                            Missing = write.object$perc[, ncol(write.object$perc)],
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$v.perc <- NULL
            names(write.object) <- c("Freq", "Perc")

          # Missing data
          } else {

            write.object$freq <- data.frame(write.object$freq[, "Var"],
                                            write.object$freq[, -c(1L, ncol(write.object$freq))],
                                            Total = rowSums(write.object$freq[, -c(1L, ncol(write.object$freq))]),
                                            Missing = write.object$freq[, ncol(write.object$freq)],
                                            Total = rowSums(write.object$freq[, -1L]),
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$perc <- data.frame(write.object$perc[, "Var"],
                                            write.object$perc[, -c(1L, ncol(write.object$perc))],
                                            Total = rowSums(write.object$perc[, -c(1L, ncol(write.object$perc))]),
                                            Missing = write.object$perc[, ncol(write.object$perc)],
                                            Total = rowSums(write.object$perc[, -1L]),
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$v.perc <- data.frame(write.object$v.perc,
                                              Total = rowSums(write.object$v.perc[, -1L]),
                                              fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            names(write.object) <- c("Freq", "Perc", "Valid Perc")

          }

        #···················
        #### Values Shown in Rows, Variables in the Columns ####

        } else {

          # Complete data
          if (isTRUE(all(!is.na(x$data)))) {

            write.object$freq <- data.frame(c("Value", rep("", times = nrow(write.object$freq) - 2L), "Total", "Missing"),
                                            c(write.object$freq[, "Value"], NA),
                                            rbind(write.object$freq[1:nrow(write.object$freq) - 1L, -1L],
                                                  colSums(write.object$freq[1:nrow(write.object$freq) - 1L, -1L]),
                                                  write.object$freq[nrow(write.object$freq), -1L]),
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$perc <- data.frame(c("Value", rep("", times = nrow(write.object$perc) - 2L), "Total", "Missing"),
                                            c(write.object$perc[, "Value"], NA),
                                            rbind(write.object$perc[1:nrow(write.object$perc) - 1L, -1L],
                                                  colSums(write.object$perc[1:nrow(write.object$perc) - 1L, -1L]),
                                                  write.object$perc[nrow(write.object$perc), -1L]),
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$v.perc <- NULL
            names(write.object) <- c("Freq", "Perc")

          # Missing data
          } else {

            write.object$freq <- data.frame(c("Value", rep("", times = nrow(write.object$freq) - 2L), "Total", "Missing", "Total"),
                                            c(write.object$freq[, "Value"], NA, NA),
                                            rbind(write.object$freq[1:nrow(write.object$freq) - 1L, -1L],
                                                  colSums(write.object$freq[1:nrow(write.object$freq) - 1L, -1L]),
                                                  write.object$freq[nrow(write.object$freq), -1L], colSums(write.object$freq[, -1L])),
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$perc <- data.frame(c("Value", rep("", times = nrow(write.object$perc) - 2L), "Total", "Missing", "Total"),
                                            c(write.object$perc[, "Value"], NA, NA),
                                            rbind(write.object$perc[1:nrow(write.object$perc) - 1L, -1L],
                                                  colSums(write.object$perc[1:nrow(write.object$perc) - 1L, -1L]),
                                                  write.object$perc[nrow(write.object$perc), -1L], colSums(write.object$perc[, -1L])),
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$v.perc <- data.frame(c("Value", rep("", times = nrow(write.object$v.perc) - 1L), "Total"),
                                              c(write.object$v.perc[, "Value"], NA),
                                              rbind(write.object$v.perc[1:nrow(write.object$v.perc), -1L],
                                                    colSums(write.object$v.perc[1:nrow(write.object$v.perc), -1L])),
                                              fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            names(write.object) <- c("Freq", "Perc", "Valid Perc")

          }

        }

      }

      # Round
      for (i in names(write.object)) {

        write.object[[i]][, !sapply(write.object[[i]], is.character)] <- sapply(write.object[[i]][, !sapply(write.object[[i]], is.character)], round, digits = digits)

      }

    }

    # Print
    if (isTRUE(x$args == "no")) {

      write.object$Perc <- NULL
      write.object$`Valid Perc` <- NULL

    } else {

      if (isTRUE(!"perc" %in% write)) { write.object$Perc <- NULL }
      if (isTRUE(!"v.perc" %in% write)) { write.object$`Valid Perc` <- NULL }

    }

  #_____________________________________________________________________________
  #
  # Coefficient Alpha and Item Statistics, item.alpha() ------------------------

  }, item.alpha = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Alpha ####

    alpha <- NULL
    if (isTRUE(!is.null(write.object$alpha))) {

      # Extract result
      alpha <- write.object$alpha

      #—————————————————————————————————————— #
      ### Round ####

      alpha[, c("alpha", "low", "upp")] <- round(alpha[, c("alpha", "low", "upp")], digits = r.digits)

      #—————————————————————————————————————— #
      ### Variable Names ####

      colnames(alpha) <- c("n", "nNA", "nItems", "Alpha", "Low", "Upp")

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Item Statistics ####

    itemstat <- NULL
    if (isTRUE(!is.null(write.object$itemstat))) {

      # Extract result
      itemstat <- write.object$itemstat

      #—————————————————————————————————————— #
      ### Round ####

      itemstat[, c("m", "sd", "min", "max")] <- round(itemstat[, c("m", "sd", "min", "max")], digits = digits)

      itemstat[, c("alpha", "d.alpha")] <- round(itemstat[, c("alpha", "d.alpha")], digits = r.digits)

      if (isTRUE("r" %in% colnames(itemstat))) {

        itemstat$r <- round(itemstat$r, digits = r.digits)

      } else {

        itemstat$std.ld<- round(itemstat$std.ld, digits = r.digits)

      }

      #—————————————————————————————————————— #
      ### Variable Names ####

      if (isTRUE("r" %in% colnames(itemstat))) {

        colnames(itemstat) <- c("Item", "n", "nNA", "%NA", "M", "SD", "Min", "Max", "r", "Alpha", "dAlpha")

      } else {

        colnames(itemstat) <- c("Item", "n", "nNA", "%NA", "M", "SD", "Min", "Max", "Std.Ld", "Alpha", "dAlpha")

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Write Object ####

    write.object <- list(Alpha = alpha, Itemstat = itemstat)

  #_____________________________________________________________________________
  #
  # Confirmatory Factor Analysis, item.cfa() -----------------------------------

  }, item.cfa = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## lavaan Summary # ####

    summary <- NULL
    if (isTRUE("summary" %in% write && !is.null(write.object$summary))) {

      # Column names
      colnames(write.object$summary) <- c(write.object$summary[1L, 1L], "", "")

      summary <- write.object$summary[-1L, ]

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Covariance Coverage # ####

    coverage <- NULL
    if (isTRUE("coverage" %in% write && !is.null(write.object$coverage))) {

      # Round
      write.object$coverage <- sapply(data.frame(write.object$coverage), round, digits = digits)

      # Add variable names in the rows
      coverage <- data.frame(colnames(write.object$coverage), write.object$coverage,
                             row.names = NULL, check.rows = FALSE,
                             check.names = FALSE, fix.empty.names = FALSE)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Univariate Sample Statistics # ####

    itemstat <- itemfreq <- NULL
    if (isTRUE("descript" %in% write && (!is.null(write.object$descript) || !is.null(write.object$itemfreq)))) {

      #—————————————————————————————————————— #
      ### Continuous Indicators ####

      if (isTRUE(!x$args$ordered)) {

        itemstat <- write.object$descript

        # Round
        itemstat[, -1L] <- sapply(itemstat[, -1L], round, digits = digits)

        colnames(itemstat) <- c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "Skew", "Kurt")

      #—————————————————————————————————————— #
      ### Univariate Counts for Ordered-Categorical Indicators ####

      } else {

        itemfreq <- write.object$itemfreq$freq

        colnames(itemfreq)[1L] <- "Variable"

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Model Fit ####

    fit <- NULL
    if (isTRUE("fit" %in% write && !is.null(write.object$fit))) {

      fit <- write.object$fit

      # Round
      fit[, -1L] <- sapply(fit[, -1L], round, digits = digits)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Parameter Estimates ####

    param <- NULL
    if (isTRUE("est" %in% write && !is.null(write.object$param))) {

      param <- write.object$param[, setdiff(colnames(write.object$param), c("lhs", "op"))]

      # Round
      param[, setdiff(colnames(param), c("param", "rhs", "pvalue"))] <- round(param[, setdiff(colnames(param), c("param", "rhs", "pvalue"))], digits = digits)

      if (isTRUE("pvalue" %in% colnames(param))) { param[, "pvalue"] <- round(param[, "pvalue"], digits = p.digits) }

      # Column names
      if (isTRUE(x$args$se  != "none")) {

        colnames(param) <-  c("Parameter", "Variable", "Estimate", "SE", "z", "p", "StdYX")

      } else {

        colnames(param) <-  c("Parameter", "Variable", "Estimate", "StdYX")

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Modification Indices ####

    modind <- NULL
    if (isTRUE("modind" %in% write && !is.null(write.object$modind))) {

      if (isTRUE(x$args$estimator != "PML")) {

        modind <- write.object$modind

        # Round
        modind[, -c(1L, 2L, 3L)] <- sapply(modind[, -c(1L, 2L, 3L)], round, digits = digits)

        colnames(modind) <- c("lhs", "op", "rhs", "MI", "EPC", "STDYX EPC")

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Residual Correlation Matrix ####

    resid <- NULL
    if (isTRUE("resid" %in% write && !is.null(write.object$resid))) {

      # Extract result table
      resid <- write.object$resid

      # Lower  Triangular
      resid[upper.tri(resid)] <- NA

      # Row names
      resid <- data.frame(row.names(resid), resid, row.names = NULL, fix.empty.names = FALSE)

      # Round
      resid[, -1L] <- sapply(resid[, -1L], round, digits = p.digits)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Relative Opdyke Distribution Percentile Matrix ####

    opdyke <- NULL
    if (isTRUE("opdyke" %in% write && !is.null(write.object$opdyke))) {

      # Extract result table
      opdyke <- write.object$opdyke

      # Lower  Triangular
      opdyke[upper.tri(opdyke)] <- NA

      # Row names
      opdyke <- data.frame(row.names(opdyke), opdyke, row.names = NULL, fix.empty.names = FALSE)

      # Round
      opdyke[, -1L] <- sapply(opdyke[, -1L], round, digits = p.digits)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Write Object ####

    write.object <- list(Summary = summary, Coverage = coverage, Itemstat = itemstat, Itemfreq = itemfreq,
                         Fit = fit, Param = param, Modind = modind, Resid = resid, Opdyke = opdyke)

  #_____________________________________________________________________________
  #
  # Dynamic Fit Index Cutoffs, item.dfi() --------------------------------------
  }, item.dfi = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## lavaan summary ####

    summary <- NULL
    if (isTRUE("summary" %in% write)) {

      # Extract result table
      summary <- write.object$summary

      # Column names
      colnames(summary) <- c(summary[1L, 1L], "")

      # Remove first row
      summary <- summary[-1L, ]

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Model Specification ####

    model <- NULL
    if (isTRUE("model" %in% write)) {

      model <- x$sim[[1L]]

      for (i in setdiff(names(x$sim.model), "Level 0")) { model <- rbind(model, "", i, as.matrix(unlist(strsplit(x$sim.model[[i]], "\n")))) }

      model <- setNames(data.frame(model, row.names = NULL), nm = "Level_0")

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Fit Index Cut-Offs ####

    fit.cutoff.output <- NULL
    if (isTRUE("cutoff" %in% write)) {

      # Extract
      fit.cutoff <- write.object$fit.cutoff

      #—————————————————————————————————————— #
      ### Suppress Cutoffs ####

      for (i in c(2L, 4L, 6L, 8L)) { fit.cutoff[which(fit.cutoff[, i] < 0.5), (i - 1L)] <- NA }

      #—————————————————————————————————————— #
      ### Round ####

      fit.cutoff[, c("cfi", "tli", "rmsea", "srmr")] <- sapply(c("cfi", "tli", "rmsea", "srmr"), function(y) round(fit.cutoff[, y], digits = digits))

      #—————————————————————————————————————— #
      ### Percent ####

      fit.cutoff[, c("power.c", "power.t", "power.r", "power.s")] <- sapply(c("power.c", "power.t", "power.r", "power.s"), function(y) fit.cutoff[, y] * 100L)

      #—————————————————————————————————————— #
      ### Combine Simulated Fit Index and Percent ####

      # Level 0
      fit.cutoff.output <- data.frame(c("Level 0", "Specificity"),
                                      rbind(setNames(fit.cutoff[1L, c("cfi", "tli", "rmsea", "srmr")], nm = c("CFI", "TLI", "RMSEA", "SRMR")), setNames(fit.cutoff[1L, c("power.c", "power.t", "power.r", "power.s")], nm = c("CFI", "TLI", "RMSEA", "SRMR"))), fix.empty.names = FALSE, row.names = NULL)

      # Level 1, 2, and 3
      if (isTRUE(nrow(fit.cutoff) > 1L)) {

        for (i in 2L:nrow(fit.cutoff)) {

          fit.cutoff.output <- rbind(fit.cutoff.output, c(rep(NA, times = 5L)),
                                     data.frame(c(rownames(fit.cutoff)[i], "Sensitivity"), rbind(setNames(fit.cutoff[i, c("cfi", "tli", "rmsea", "srmr")], nm = c("CFI", "TLI", "RMSEA", "SRMR")), setNames(fit.cutoff[i, c("power.c", "power.t", "power.r", "power.s")], nm = c("CFI", "TLI", "RMSEA", "SRMR"))), fix.empty.names = FALSE, row.names = NULL))

        }

      }

      #—————————————————————————————————————— #
      ### Attach Empirical Fit Indices ####

      if (isTRUE(!is.null(x$result$fit.emp))) {

        fit.emp <- x$result$fit.emp

        #···················
        #### Round ####

        fit.emp[c("chisq", "cfi", "tli", "rmsea", "srmr")] <- sapply(c("chisq", "cfi", "tli", "rmsea", "srmr"), function(y) round(fit.emp[y], digits = digits))

        #···················
        #### Combine Simulated Cutoffs and Empirical Fit Indices ####

        fit.cutoff.output <- rbind(data.frame(fit.cutoff.output[, 1L], cbind(Chi2 = NA, df = NA, fit.cutoff.output[, -1L]), fix.empty.names = FALSE, row.names = NULL),
                                   rep(NA, times = 7L),
                                   data.frame("Empirical Fit Indices", matrix(rep(NA, times = 6L), ncol = 6, dimnames = list(NULL, c("Chi2", "df", "CFI", "TLI", "RMSEA", "SRMR"))), fix.empty.names = FALSE),
                                   setNames(c(NA, fit.emp[c("chisq", "df", "cfi", "tli", "rmsea", "srmr")]), c("", "Chi2", "df", "CFI", "TLI", "RMSEA", "SRMR")))



      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Write object ####

    write.object <- list(Summary = summary, Model = model, Cutoff = fit.cutoff.output)

  #_____________________________________________________________________________
  #
  # Distractor Analysis for Multiple-Choice Items, item.distract() -------------
  }, item.distract = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Format ####

    #—————————————————————————————————————— #
    ### Round ####

    # Distractor selection percentage
    if (isTRUE(any(substr(colnames(write.object), 1L, 1L) == "p"))) { write.object[, substr(colnames(write.object), 1L, 1L) == "p"] <- round(write.object[, substr(colnames(write.object), 1L, 1L) == "p"], digits = digits) }

    # Attractor-distractor-total correlation
    if (isTRUE(any(substr(colnames(write.object), 1L, 1L) == "r"))) { write.object[, substr(colnames(write.object), 1L, 1L) == "r"] <- round(write.object[, substr(colnames(write.object), 1L, 1L) == "r"], digits = r.digits) }

    #—————————————————————————————————————— #
    ### Variable Name ####

    colnames(write.object) <- misty::chr.gsub(pattern = c("item", "key"), replacement = c("Item", "Key"), colnames(write.object))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Write object ####

    write.object <- list(Itemdistract = write.object)

  #_____________________________________________________________________________
  #
  # Measurement Invariance Evaluation, item.invar() ----------------------------
  }, item.invar = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## lavaan summary ####

    summary <- NULL
    if (isTRUE("summary" %in% write)) {

      # Extract result table
      summary <- write.object$summary

      # Column names
      colnames(summary) <- c(summary[1L, 1L], rep("", times = ncol(summary) - 1L))

      # Remove first row
      summary <- summary[-1L, ]

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Covariance coverage ####

    coverage <- NULL
    if (isTRUE("coverage" %in% write)) {

      # Extract result table
      coverage <- write.object$coverage

      # Between-group measurement invariance
      if (isTRUE(!x$args$long)) {

        # Combine data frames and round
        coverage <- data.frame(group = rep(names(coverage), each = nrow(coverage[[1L]])),
                               colnames(coverage[[1L]]),
                               apply(do.call("rbind", coverage), 2L, round, digits = p.digits),
                               row.names = NULL, fix.empty.names = FALSE)

      # Longitudinal measurement invariance
      } else {

        # Combine data frames and round
        coverage <- data.frame(colnames(coverage), coverage,
                               row.names = NULL, fix.empty.names = FALSE)

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Univariate Sample Statistics ####

    itemstat <- NULL
    if (isTRUE("descript" %in% write)) {

      #—————————————————————————————————————— #
      ### Continuous Indicators ####

      if (isTRUE(!x$args$ordered)) {

        # Extract result table
        itemstat <- write.object$descript$stat

        # Round
        itemstat[, c("m", "sd", "min", "max", "skew", "kurt")] <- sapply(itemstat[, c("m", "sd", "min", "max", "skew", "kurt")], round, digits = digits)
        itemstat[, "pNA"] <- round(itemstat[, "pNA"], digits = digits - 1L)

        # Column names
        colnames(itemstat) <- c(if (isTRUE(!x$args$long)) { "Group" }, "Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "Skew", "Kurt")

      #—————————————————————————————————————— #
      ### Ordered Categorical Indicators ####

      } else {

        #### Between-Group Measurement Invariance ####
        if (isTRUE(!x$args$long)) {

          # Extract result table
          itemstat <- write.object$descript$freq |> (\(p) data.frame(Group = rep(names(p), each = unique(sapply(p, nrow))), do.call("rbind", p), row.names = NULL, check.names = FALSE))() |> (\(q) misty::df.rename(q, from = "Var", to = "Variable"))()

        #### Longitudinal Measurement Invariance ####
        } else {

          itemstat <- misty::df.rename(write.object$descript$freq, from = "Var", to = "Variable")

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Model fit ####

    fit <- NULL
    if (isTRUE("fit" %in% write)) {

      # Extract result table
      fit <- write.object$fit

      # Standard fit indices
      if (isTRUE(x$args$estimator %in% c("ML", "MLF", "GLS", "WLS", "DWLS", "ULS", "PML"))) {

        # Combine data frames
        fit <- data.frame(c("Standard", rep(NA, times = nrow(fit$stand))),
                          do.call("rbind", lapply(fit, function(y) rbind(NA, y))),
                          row.names = NULL, fix.empty.names = FALSE)

      # Standard, scaled, and robust fit indices
      } else {

        # Combine data frames
        fit <- data.frame(c("Standard", rep(NA, times = nrow(fit$stand)), "Scaled", rep(NA, times = nrow(fit$scaled)), "Robust", rep(NA, times = nrow(fit$robust))),
                          do.call("rbind", lapply(fit, function(y) rbind(NA, y))),
                          row.names = NULL, fix.empty.names = FALSE)

      }

      # Round
      fit[which(!fit[, 2L] %in% c("P-value", "P-value RMSEA <= 0.05")), c(3L:ncol(fit))] <- sapply(fit[which(!fit[, 2L] %in% c("P-value", "P-value RMSEA <= 0.05")), c(3L:ncol(fit))], round, digits = digits)
      fit[which(fit[, 2L] %in% c("P-value", "P-value RMSEA <= 0.05")), c(3L:ncol(fit))] <- sapply(fit[which(fit[, 2L] %in% c("P-value", "P-value RMSEA <= 0.05")), c(3L:ncol(fit))], round, digits = p.digits)

      #—————————————————————————————————————— #
      ### Continuous Indicators ####

      if (isTRUE(!x$args$ordered)) {

        # Column names
        switch(x$args$invar,
               config = { colnames(fit) <- c("", "", "Config") },
               metric = { colnames(fit) <- c("", "", "Config", "Metric", "dMetric") },
               scalar = { colnames(fit) <- c("", "", "Config", "Metric", "Scalar", "dMetric", "dScalar") },
               strict = { colnames(fit) <- c("", "", "Config", "Metric", "Scalar", "Stict", "dMetric", "dScalar", "dStrict") })

      #—————————————————————————————————————— #
      ### Ordered Categorical Indicators ####

      } else {

        # Column names
        switch(x$args$invar,
               config = { colnames(fit) <- c("", "", "Config") },
               thres  = { colnames(fit) <- c("", "", "Config", "Thres", "dThres") },
               metric = { colnames(fit) <- c("", "", "Config", "Thres", "Metric", "dMetric") },
               scalar = { colnames(fit) <- c("", "", "Config", "Thres", "Metric", "Scalar", "dThres", "dMetric", "dScalar") },
               strict = { colnames(fit) <- c("", "", "Config", "Thres", "Metric", "Scalar", "Stict", "dThres", "dMetric", "dScalar", "dStrict") })

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Parameter estimates ####

    param <- NULL
    if (isTRUE("est" %in% write)) {

      # Extract result table
      param <- write.object$param |> (\(p) p[sapply(p, function(y) !is.null(y))])()

      #—————————————————————————————————————— #
      ### Continuous Indicators ####

      if (isTRUE(!x$args$ordered)) {

        # Combine data frames
        param <- data.frame(switch(x$args$invar,
                                   config = { c("Config", rep(NA, times = nrow(param$config))) },
                                   metric = { c("Config", rep(NA, times = nrow(param$config)), "Metric", rep(NA, times = nrow(param$metric))) },
                                   scalar = { c("Config", rep(NA, times = nrow(param$config)), "Metric", rep(NA, times = nrow(param$metric)), "Scalar", rep(NA, times = nrow(param$scalar))) },
                                   strict = { c("Config", rep(NA, times = nrow(param$config)), "Metric", rep(NA, times = nrow(param$metric)), "Scalar", rep(NA, times = nrow(param$scalar)), "Stict", rep(NA, times = nrow(param$strict))) }),
                            do.call("rbind", lapply(param, function(y) rbind(NA, y))),
                            row.names = NULL, fix.empty.names = FALSE)

      #—————————————————————————————————————— #
      ### Ordered Categorical Indicators ####

      } else {

        # Combine data frames
        param <- data.frame(switch(x$args$invar,
                                   config = { c("Config", rep(NA, times = nrow(param$config))) },
                                   thres  = { c("Config", rep(NA, times = nrow(param$config)), "Thres", rep(NA, times = nrow(param$thres))) },
                                   metric = { c("Config", rep(NA, times = nrow(param$config)), "Thres", rep(NA, times = nrow(param$thres)), "Metric", rep(NA, times = nrow(param$metric))) },
                                   scalar = { c("Config", rep(NA, times = nrow(param$config)), "Thres", rep(NA, times = nrow(param$thres)), "Metric", rep(NA, times = nrow(param$metric)), "Scalar", rep(NA, times = nrow(param$scalar))) },
                                   strict = { c("Config", rep(NA, times = nrow(param$config)), "Thres", rep(NA, times = nrow(param$thres)), "Metric", rep(NA, times = nrow(param$metric)), "Scalar", rep(NA, times = nrow(param$scalar)), "Stict", rep(NA, times = nrow(param$strict))) }),
                            do.call("rbind", lapply(param, function(y) rbind(NA, y))),
                            row.names = NULL, fix.empty.names = FALSE)

      }

      # Round
      param[, c("est", "se", "z", "stdyx")] <- sapply(param[, c("est", "se", "z", "stdyx")], round, digits = digits)
      param[, "pvalue"] <- round(param[, "pvalue"], digits = p.digits)

      # Column names
      colnames(param) <- c("", "Parameter", if (isTRUE(!x$args$long)) { "Group" }, "lhs", "op", "rhs", "label", "Estimate", "SE", "z", "pvalue", "StdYX")

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Modification indices ####

    modind <- NULL
    if (isTRUE("modind" %in% write && any(!sapply(write.object$modind, is.null)))) {

      # Extract result table
      modind <- write.object$modind |> (\(p) p[sapply(p, function(y) !is.null(y))])()

      #—————————————————————————————————————— #
      ### Continuous Indicators ####

      if (isTRUE(!x$args$ordered)) {

        # Combine data frames
        modind <- data.frame(switch(x$args$invar,
                                    config = {   if (is.null(modind$config)) { NULL } else { c("Config", rep(NA, times = nrow(modind$config))) } },
                                    metric = { c(if (is.null(modind$config)) { NULL } else { c("Config", rep(NA, times = nrow(modind$config))) },
                                                 if (is.null(modind$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(modind$metric))) }) },
                                    scalar = { c(if (is.null(modind$config)) { NULL } else { c("Config", rep(NA, times = nrow(modind$config))) },
                                                 if (is.null(modind$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(modind$metric))) },
                                                 if (is.null(modind$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(modind$scalar))) }) },
                                    strict = { c(if (is.null(modind$config)) { NULL } else { c("Config", rep(NA, times = nrow(modind$config))) },
                                                 if (is.null(modind$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(modind$metric))) },
                                                 if (is.null(modind$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(modind$scalar))) },
                                                 if (is.null(modind$strict)) { NULL } else { c("strict", rep(NA, times = nrow(modind$strict))) }) }),
                             do.call("rbind", lapply(modind, function(y) rbind(NA, y))),
                             row.names = NULL, fix.empty.names = FALSE)

      #—————————————————————————————————————— #
      ### Ordered Categorical Indicators ####

      } else {

        # Combine data frames
        modind <- data.frame(switch(x$args$invar,
                                    config = {   if (is.null(modind$config)) { NULL } else { c("Config", rep(NA, times = nrow(modind$config))) } },
                                    thres = {  c(if (is.null(modind$config)) { NULL } else { c("Config", rep(NA, times = nrow(modind$config))) },
                                                 if (is.null(modind$thres))  { NULL } else { c("Thres",  rep(NA, times = nrow(modind$thres))) }) },
                                    metric = { c(if (is.null(modind$config)) { NULL } else { c("Config", rep(NA, times = nrow(modind$config))) },
                                                 if (is.null(modind$thres))  { NULL } else { c("Thres",  rep(NA, times = nrow(modind$thres))) },
                                                 if (is.null(modind$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(modind$metric))) }) },
                                    scalar = { c(if (is.null(modind$config)) { NULL } else { c("Config", rep(NA, times = nrow(modind$config))) },
                                                 if (is.null(modind$thres))  { NULL } else { c("Thres",  rep(NA, times = nrow(modind$thres))) },
                                                 if (is.null(modind$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(modind$metric))) },
                                                 if (is.null(modind$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(modind$scalar))) }) },
                                    strict = { c(if (is.null(modind$config)) { NULL } else { c("Config", rep(NA, times = nrow(modind$config))) },
                                                 if (is.null(modind$thres))  { NULL } else { c("Thres",  rep(NA, times = nrow(modind$thres))) },
                                                 if (is.null(modind$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(modind$metric))) },
                                                 if (is.null(modind$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(modind$scalar))) },
                                                 if (is.null(modind$strict)) { NULL } else { c("strict", rep(NA, times = nrow(modind$strict))) }) }),
                             do.call("rbind", lapply(modind, function(y) rbind(NA, y))),
                             row.names = NULL, fix.empty.names = FALSE)

      }

      # Round
      modind[, c("mi", "epc", "stdyx")] <- sapply(modind[, c("mi", "epc", "stdyx")], round, digits = digits)

      # Column names
      colnames(modind) <- c("", if (isTRUE(!x$args$long)) { "Group" }, "lhs", "op", "rhs", "MI", "EPC", "StdYX")

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Modification Indices for Parameter Constraints ####

    score <- NULL
    if (isTRUE("modind" %in% write && any(!sapply(write.object$score, is.null)))) {

      # Extract result table
      score <- write.object$score |> (\(p) p[sapply(p, function(y) !is.null(y))])()

      #—————————————————————————————————————— #
      ### Continuous Indicators ####

      if (isTRUE(!x$args$ordered)) {

        # Combine data frames
        score <- data.frame(switch(x$args$invar,
                                   config = {   if (is.null(score$config)) { NULL } else { c("Config", rep(NA, times = nrow(score$config))) } },
                                   metric = { c(if (is.null(score$config)) { NULL } else { c("Config", rep(NA, times = nrow(score$config))) },
                                                if (is.null(score$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(score$metric))) }) },
                                   scalar = { c(if (is.null(score$config)) { NULL } else { c("Config", rep(NA, times = nrow(score$config))) },
                                                if (is.null(score$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(score$metric))) },
                                                if (is.null(score$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(score$scalar))) }) },
                                   strict = { c(if (is.null(score$config)) { NULL } else { c("Config", rep(NA, times = nrow(score$config))) },
                                                if (is.null(score$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(score$metric))) },
                                                if (is.null(score$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(score$scalar))) },
                                                if (is.null(score$strict)) { NULL } else { c("strict", rep(NA, times = nrow(score$strict))) }) }),
                            do.call("rbind", lapply(score, function(y) rbind(NA, y))),
                            row.names = NULL, fix.empty.names = FALSE)

      #—————————————————————————————————————— #
      ### Ordered Categorical Indicators ####

      } else {

        # Combine data frames
        score <- data.frame(switch(x$args$invar,
                                   config = {   if (is.null(score$config)) { NULL } else { c("Config", rep(NA, times = nrow(score$config))) } },
                                   thres =  { c(if (is.null(score$config)) { NULL } else { c("Config", rep(NA, times = nrow(score$config))) },
                                                if (is.null(score$thres))  { NULL } else { c("Thres",  rep(NA, times = nrow(score$thres))) }) },
                                   metric = { c(if (is.null(score$config)) { NULL } else { c("Config", rep(NA, times = nrow(score$config))) },
                                                if (is.null(score$thres))  { NULL } else { c("Thres",  rep(NA, times = nrow(score$thres))) },
                                                if (is.null(score$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(score$metric))) }) },
                                   scalar = { c(if (is.null(score$config)) { NULL } else { c("Config", rep(NA, times = nrow(score$config))) },
                                                if (is.null(score$thres))  { NULL } else { c("Thres",  rep(NA, times = nrow(score$thres))) },
                                                if (is.null(score$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(score$metric))) },
                                                if (is.null(score$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(score$scalar))) }) },
                                   strict = { c(if (is.null(score$config)) { NULL } else { c("Config", rep(NA, times = nrow(score$config))) },
                                                if (is.null(score$thres))  { NULL } else { c("Thres",  rep(NA, times = nrow(score$thres))) },
                                                if (is.null(score$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(score$metric))) },
                                                if (is.null(score$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(score$scalar))) },
                                                if (is.null(score$strict)) { NULL } else { c("strict", rep(NA, times = nrow(score$strict))) }) }),
                            do.call("rbind", lapply(score, function(y) rbind(NA, y))),
                            row.names = NULL, fix.empty.names = FALSE)

      }

      # Round
      score[, c("mi", "lhs.epc", "rhs.epc", "lhs.stdyx", "rhs.stdyx")] <- sapply(score[, c("mi", "lhs.epc", "rhs.epc", "lhs.stdyx", "rhs.stdyx")], round, digits = digits)
      score[, "pvalue"] <- round(score[, "pvalue"], digits = p.digits)

      # Column names
      colnames(score) <- c("", "Label", if (isTRUE(!x$args$long)) { c("Group.lhs", "Group.rhs") }, "lhs", "op", "rhs", "MI", "df", "pvalue", "lhs.EPC", "rhs.EPC", "lhs.StdYX", "rhs.StdYX")

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Residual Correlation Matrix ####

    resid <- NULL
    if (isTRUE("resid" %in% write && any(!sapply(write.object$resid, is.null)))) {

      # Extract result table
      resid <- write.object$resid |> (\(p) p[sapply(p, function(y) !is.null(y))])()

      #—————————————————————————————————————— #
      ### Between-Group Measurement Invariance ####

      if (isTRUE(!x$args$long)) {

        #### Continuous Indicators ####

        if (isTRUE(!x$args$ordered)) {

          resid <- data.frame(switch(x$args$invar,
                                     config = {   if (is.null(resid$config)) { NULL } else { rep(c("Config", rep(NA, times = nrow(resid$config[[1L]]))), times = length(resid$config)) } },
                                     metric = { c(if (is.null(resid$config)) { NULL } else { rep(c("Config", rep(NA, times = nrow(resid$config[[1L]]))), times = length(resid$config)) },
                                                  if (is.null(resid$metric)) { NULL } else { rep(c("Metric", rep(NA, times = nrow(resid$metric[[1L]]))), times = length(resid$metric)) }) },
                                     scalar = { c(if (is.null(resid$config)) { NULL } else { rep(c("Config", rep(NA, times = nrow(resid$config[[1L]]))), times = length(resid$config)) },
                                                  if (is.null(resid$metric)) { NULL } else { rep(c("Metric", rep(NA, times = nrow(resid$metric[[1L]]))), times = length(resid$metric)) },
                                                  if (is.null(resid$scalar)) { NULL } else { rep(c("Scalar", rep(NA, times = nrow(resid$scalar[[1L]]))), times = length(resid$scalar)) }) },
                                     strict = { c(if (is.null(resid$config)) { NULL } else { rep(c("Config", rep(NA, times = nrow(resid$config[[1L]]))), times = length(resid$config)) },
                                                  if (is.null(resid$metric)) { NULL } else { rep(c("Metric", rep(NA, times = nrow(resid$metric[[1L]]))), times = length(resid$metric)) },
                                                  if (is.null(resid$scalar)) { NULL } else { rep(c("Scalar", rep(NA, times = nrow(resid$scalar[[1L]]))), times = length(resid$scalar)) },
                                                  if (is.null(resid$strict)) { NULL } else { rep(c("strict", rep(NA, times = nrow(resid$strict[[1L]]))), times = length(resid$strict)) }) }),
                              do.call("rbind", lapply(lapply(resid, function(y) do.call("rbind", lapply(y, function(z) rbind(NA, z)))), function(q) data.frame(rep(names(resid[[1L]]), each = nrow(resid[[1L]][[1L]]) + 1L), c("", row.names(resid[[1L]][[1L]])), q, fix.empty.names = FALSE))),
                              row.names = NULL, fix.empty.names = FALSE)

        #### Ordered Categorical Indicators ####

        } else {

          resid <- data.frame(switch(x$args$invar,
                                     config = {   if (is.null(resid$config)) { NULL } else { rep(c("Config", rep(NA, times = nrow(resid$config[[1L]]))), times = length(resid$config)) } },
                                     thres  = { c(if (is.null(resid$config)) { NULL } else { rep(c("Config", rep(NA, times = nrow(resid$config[[1L]]))), times = length(resid$config)) },
                                                  if (is.null(resid$thres))  { NULL } else { rep(c("Thres", rep(NA, times = nrow(resid$thres[[1L]]))), times = length(resid$thres)) }) },
                                     metric = { c(if (is.null(resid$config)) { NULL } else { rep(c("Config", rep(NA, times = nrow(resid$config[[1L]]))), times = length(resid$config)) },
                                                  if (is.null(resid$thres))  { NULL } else { rep(c("Thres", rep(NA, times = nrow(resid$thres[[1L]]))), times = length(resid$thres)) },
                                                  if (is.null(resid$metric)) { NULL } else { rep(c("Metric", rep(NA, times = nrow(resid$metric[[1L]]))), times = length(resid$metric)) }) },
                                     scalar = { c(if (is.null(resid$config)) { NULL } else { rep(c("Config", rep(NA, times = nrow(resid$config[[1L]]))), times = length(resid$config)) },
                                                  if (is.null(resid$thres))  { NULL } else { rep(c("Thres", rep(NA, times = nrow(resid$thres[[1L]]))), times = length(resid$thres)) },
                                                  if (is.null(resid$metric)) { NULL } else { rep(c("Metric", rep(NA, times = nrow(resid$metric[[1L]]))), times = length(resid$metric)) },
                                                  if (is.null(resid$scalar)) { NULL } else { rep(c("Scalar", rep(NA, times = nrow(resid$scalar[[1L]]))), times = length(resid$scalar)) }) },
                                     strict = { c(if (is.null(resid$config)) { NULL } else { rep(c("Config", rep(NA, times = nrow(resid$config[[1L]]))), times = length(resid$config)) },
                                                  if (is.null(resid$thres))  { NULL } else { rep(c("Thres", rep(NA, times = nrow(resid$thres[[1L]]))), times = length(resid$thres)) },
                                                  if (is.null(resid$metric)) { NULL } else { rep(c("Metric", rep(NA, times = nrow(resid$metric[[1L]]))), times = length(resid$metric)) },
                                                  if (is.null(resid$scalar)) { NULL } else { rep(c("Scalar", rep(NA, times = nrow(resid$scalar[[1L]]))), times = length(resid$scalar)) },
                                                  if (is.null(resid$strict)) { NULL } else { rep(c("strict", rep(NA, times = nrow(resid$strict[[1L]]))), times = length(resid$strict)) }) }),
                              do.call("rbind", lapply(lapply(resid, function(y) do.call("rbind", lapply(y, function(z) rbind(NA, z)))), function(q) data.frame(rep(names(resid[[1L]]), each = nrow(resid[[1L]][[1L]]) + 1L), c("", row.names(resid[[1L]][[1L]])), q, fix.empty.names = FALSE))),
                              row.names = NULL, fix.empty.names = FALSE)

          }

        # Round
        resid[, -c(1L:3L)] <- sapply(resid[, -c(1L:3L)], round, digits = p.digits)

        # Column names
        colnames(resid) <- c("", if (isTRUE(!x$args$long)) { "Group" }, colnames(resid)[-c(1L:2L)])

      #—————————————————————————————————————— #
      ### Longitudinal Measurement Invariance ####

      } else {

        #### Continuous Indicators ####

        if (isTRUE(!x$args$ordered)) {

          resid <- data.frame(switch(x$args$invar,
                                     config = {   if (is.null(resid$config)) { NULL } else { c("Config", rep(NA, times = nrow(resid$config))) } },
                                     metric = { c(if (is.null(resid$config)) { NULL } else { c("Config", rep(NA, times = nrow(resid$config))) },
                                                  if (is.null(resid$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(resid$metric))) }) },
                                     scalar = { c(if (is.null(resid$config)) { NULL } else { c("Config", rep(NA, times = nrow(resid$config))) },
                                                  if (is.null(resid$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(resid$metric))) },
                                                  if (is.null(resid$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(resid$scalar))) }) },
                                     strict = { c(if (is.null(resid$config)) { NULL } else { c("Config", rep(NA, times = nrow(resid$config))) },
                                                  if (is.null(resid$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(resid$metric))) },
                                                  if (is.null(resid$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(resid$scalar))) },
                                                  if (is.null(resid$strict)) { NULL } else { c("strict", rep(NA, times = nrow(resid$strict))) }) }),
                              data.frame(c(NA, rownames(resid$config)), do.call("rbind", lapply(resid, function(y) rbind(NA, y))),
                                         row.names = NULL, fix.empty.names = FALSE), row.names = NULL, fix.empty.names = FALSE)

        #### Ordered Categorical Indicators ####

        } else {

          resid <- data.frame(switch(x$args$invar,
                                     config = {   if (is.null(resid$config)) { NULL } else { c("Config", rep(NA, times = nrow(resid$config))) } },
                                     thres =  { c(if (is.null(resid$config)) { NULL } else { c("Config", rep(NA, times = nrow(resid$config))) },
                                                  if (is.null(resid$thres))  { NULL } else { c("Thres", rep(NA, times = nrow(resid$thres))) }) },
                                     metric = { c(if (is.null(resid$config)) { NULL } else { c("Config", rep(NA, times = nrow(resid$config))) },
                                                  if (is.null(resid$thres))  { NULL } else { c("Thres", rep(NA, times = nrow(resid$thres))) },
                                                  if (is.null(resid$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(resid$metric))) }) },
                                     scalar = { c(if (is.null(resid$config)) { NULL } else { c("Config", rep(NA, times = nrow(resid$config))) },
                                                  if (is.null(resid$thres))  { NULL } else { c("Thres", rep(NA, times = nrow(resid$thres))) },
                                                  if (is.null(resid$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(resid$metric))) },
                                                  if (is.null(resid$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(resid$scalar))) }) },
                                     strict = { c(if (is.null(resid$config)) { NULL } else { c("Config", rep(NA, times = nrow(resid$config))) },
                                                  if (is.null(resid$thres))  { NULL } else { c("Thres", rep(NA, times = nrow(resid$thres))) },
                                                  if (is.null(resid$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(resid$metric))) },
                                                  if (is.null(resid$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(resid$scalar))) },
                                                  if (is.null(resid$strict)) { NULL } else { c("strict", rep(NA, times = nrow(resid$strict))) }) }),
                              data.frame(c(NA, rownames(resid$config)), do.call("rbind", lapply(resid, function(y) rbind(NA, y))),
                                         row.names = NULL, fix.empty.names = FALSE), row.names = NULL, fix.empty.names = FALSE)

        }

        # Round
        resid[, -c(1L:2L)] <- sapply(resid[, -c(1L:2L)], round, digits = p.digits)

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Relative Opdyke Distribution Percentile Matrix ####

    opdyke <- NULL
    if (isTRUE("opdyke" %in% write && any(!sapply(write.object$opdyke, is.null)))) {

      # Extract result table
      opdyke <- write.object$opdyke |> (\(p) p[sapply(p, function(y) !is.null(y))])()

      #—————————————————————————————————————— #
      ### Between-Group Measurement Invariance ####

      if (isTRUE(!x$args$long)) {

        #### Continuous Indicators ####

        if (isTRUE(!x$args$ordered)) {

          opdyke <- data.frame(switch(x$args$invar,
                                      config = {   if (is.null(opdyke$config)) { NULL } else { rep(c("Config", rep(NA, times = nrow(opdyke$config[[1L]]))), times = length(opdyke$config)) } },
                                      metric = { c(if (is.null(opdyke$config)) { NULL } else { rep(c("Config", rep(NA, times = nrow(opdyke$config[[1L]]))), times = length(opdyke$config)) },
                                                   if (is.null(opdyke$metric)) { NULL } else { rep(c("Metric", rep(NA, times = nrow(opdyke$metric[[1L]]))), times = length(opdyke$metric)) }) },
                                      scalar = { c(if (is.null(opdyke$config)) { NULL } else { rep(c("Config", rep(NA, times = nrow(opdyke$config[[1L]]))), times = length(opdyke$config)) },
                                                   if (is.null(opdyke$metric)) { NULL } else { rep(c("Metric", rep(NA, times = nrow(opdyke$metric[[1L]]))), times = length(opdyke$metric)) },
                                                   if (is.null(opdyke$scalar)) { NULL } else { rep(c("Scalar", rep(NA, times = nrow(opdyke$scalar[[1L]]))), times = length(opdyke$scalar)) }) },
                                      strict = { c(if (is.null(opdyke$config)) { NULL } else { rep(c("Config", rep(NA, times = nrow(opdyke$config[[1L]]))), times = length(opdyke$config)) },
                                                   if (is.null(opdyke$metric)) { NULL } else { rep(c("Metric", rep(NA, times = nrow(opdyke$metric[[1L]]))), times = length(opdyke$metric)) },
                                                   if (is.null(opdyke$scalar)) { NULL } else { rep(c("Scalar", rep(NA, times = nrow(opdyke$scalar[[1L]]))), times = length(opdyke$scalar)) },
                                                   if (is.null(opdyke$strict)) { NULL } else { rep(c("strict", rep(NA, times = nrow(opdyke$strict[[1L]]))), times = length(opdyke$strict)) }) }),
                               do.call("rbind", lapply(lapply(opdyke, function(y) do.call("rbind", lapply(y, function(z) rbind(NA, z)))), function(q) data.frame(rep(names(opdyke[[1L]]), each = nrow(opdyke[[1L]][[1L]]) + 1L), c("", row.names(opdyke[[1L]][[1L]])), q, fix.empty.names = FALSE))),
                               row.names = NULL, fix.empty.names = FALSE)

          #### Ordered Categorical Indicators ####

        } else {

          opdyke <- data.frame(switch(x$args$invar,
                                      config = {   if (is.null(opdyke$config)) { NULL } else { rep(c("Config", rep(NA, times = nrow(opdyke$config[[1L]]))), times = length(opdyke$config)) } },
                                      thres  = { c(if (is.null(opdyke$config)) { NULL } else { rep(c("Config", rep(NA, times = nrow(opdyke$config[[1L]]))), times = length(opdyke$config)) },
                                                   if (is.null(opdyke$thres))  { NULL } else { rep(c("Thres", rep(NA, times = nrow(opdyke$thres[[1L]]))), times = length(opdyke$thres)) }) },
                                      metric = { c(if (is.null(opdyke$config)) { NULL } else { rep(c("Config", rep(NA, times = nrow(opdyke$config[[1L]]))), times = length(opdyke$config)) },
                                                   if (is.null(opdyke$thres))  { NULL } else { rep(c("Thres", rep(NA, times = nrow(opdyke$thres[[1L]]))), times = length(opdyke$thres)) },
                                                   if (is.null(opdyke$metric)) { NULL } else { rep(c("Metric", rep(NA, times = nrow(opdyke$metric[[1L]]))), times = length(opdyke$metric)) }) },
                                      scalar = { c(if (is.null(opdyke$config)) { NULL } else { rep(c("Config", rep(NA, times = nrow(opdyke$config[[1L]]))), times = length(opdyke$config)) },
                                                   if (is.null(opdyke$thres))  { NULL } else { rep(c("Thres", rep(NA, times = nrow(opdyke$thres[[1L]]))), times = length(opdyke$thres)) },
                                                   if (is.null(opdyke$metric)) { NULL } else { rep(c("Metric", rep(NA, times = nrow(opdyke$metric[[1L]]))), times = length(opdyke$metric)) },
                                                   if (is.null(opdyke$scalar)) { NULL } else { rep(c("Scalar", rep(NA, times = nrow(opdyke$scalar[[1L]]))), times = length(opdyke$scalar)) }) },
                                      strict = { c(if (is.null(opdyke$config)) { NULL } else { rep(c("Config", rep(NA, times = nrow(opdyke$config[[1L]]))), times = length(opdyke$config)) },
                                                   if (is.null(opdyke$thres))  { NULL } else { rep(c("Thres", rep(NA, times = nrow(opdyke$thres[[1L]]))), times = length(opdyke$thres)) },
                                                   if (is.null(opdyke$metric)) { NULL } else { rep(c("Metric", rep(NA, times = nrow(opdyke$metric[[1L]]))), times = length(opdyke$metric)) },
                                                   if (is.null(opdyke$scalar)) { NULL } else { rep(c("Scalar", rep(NA, times = nrow(opdyke$scalar[[1L]]))), times = length(opdyke$scalar)) },
                                                   if (is.null(opdyke$strict)) { NULL } else { rep(c("strict", rep(NA, times = nrow(opdyke$strict[[1L]]))), times = length(opdyke$strict)) }) }),
                               do.call("rbind", lapply(lapply(opdyke, function(y) do.call("rbind", lapply(y, function(z) rbind(NA, z)))), function(q) data.frame(rep(names(opdyke[[1L]]), each = nrow(opdyke[[1L]][[1L]]) + 1L), c("", row.names(opdyke[[1L]][[1L]])), q, fix.empty.names = FALSE))),
                               row.names = NULL, fix.empty.names = FALSE)

        }

        # Round
        opdyke[, -c(1L:3L)] <- sapply(opdyke[, -c(1L:3L)], round, digits = p.digits)

        # Column names
        colnames(opdyke) <- c("", if (isTRUE(!x$args$long)) { "Group" }, colnames(opdyke)[-c(1L:2L)])

      #—————————————————————————————————————— #
      ### Longitudinal Measurement Invariance ####

      } else {

        #### Continuous Indicators ####

        if (isTRUE(!x$args$ordered)) {

          opdyke <- data.frame(switch(x$args$invar,
                                      config = {   if (is.null(opdyke$config)) { NULL } else { c("Config", rep(NA, times = nrow(opdyke$config))) } },
                                      metric = { c(if (is.null(opdyke$config)) { NULL } else { c("Config", rep(NA, times = nrow(opdyke$config))) },
                                                   if (is.null(opdyke$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(opdyke$metric))) }) },
                                      scalar = { c(if (is.null(opdyke$config)) { NULL } else { c("Config", rep(NA, times = nrow(opdyke$config))) },
                                                   if (is.null(opdyke$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(opdyke$metric))) },
                                                   if (is.null(opdyke$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(opdyke$scalar))) }) },
                                      strict = { c(if (is.null(opdyke$config)) { NULL } else { c("Config", rep(NA, times = nrow(opdyke$config))) },
                                                   if (is.null(opdyke$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(opdyke$metric))) },
                                                   if (is.null(opdyke$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(opdyke$scalar))) },
                                                   if (is.null(opdyke$strict)) { NULL } else { c("strict", rep(NA, times = nrow(opdyke$strict))) }) }),
                               data.frame(c(NA, rownames(opdyke$config)), do.call("rbind", lapply(opdyke, function(y) rbind(NA, y))),
                                          row.names = NULL, fix.empty.names = FALSE), row.names = NULL, fix.empty.names = FALSE)

        #### Ordered Categorical Indicators ####

        } else {

          opdyke <- data.frame(switch(x$args$invar,
                                      config = {   if (is.null(opdyke$config)) { NULL } else { c("Config", rep(NA, times = nrow(opdyke$config))) } },
                                      thres =  { c(if (is.null(opdyke$config)) { NULL } else { c("Config", rep(NA, times = nrow(opdyke$config))) },
                                                   if (is.null(opdyke$thres))  { NULL } else { c("Thres", rep(NA, times = nrow(opdyke$thres))) }) },
                                      metric = { c(if (is.null(opdyke$config)) { NULL } else { c("Config", rep(NA, times = nrow(opdyke$config))) },
                                                   if (is.null(opdyke$thres))  { NULL } else { c("Thres", rep(NA, times = nrow(opdyke$thres))) },
                                                   if (is.null(opdyke$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(opdyke$metric))) }) },
                                      scalar = { c(if (is.null(opdyke$config)) { NULL } else { c("Config", rep(NA, times = nrow(opdyke$config))) },
                                                   if (is.null(opdyke$thres))  { NULL } else { c("Thres", rep(NA, times = nrow(opdyke$thres))) },
                                                   if (is.null(opdyke$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(opdyke$metric))) },
                                                   if (is.null(opdyke$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(opdyke$scalar))) }) },
                                      strict = { c(if (is.null(opdyke$config)) { NULL } else { c("Config", rep(NA, times = nrow(opdyke$config))) },
                                                   if (is.null(opdyke$thres))  { NULL } else { c("Thres", rep(NA, times = nrow(opdyke$thres))) },
                                                  if (is.null(opdyke$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(opdyke$metric))) },
                                                  if (is.null(opdyke$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(opdyke$scalar))) },
                                                  if (is.null(opdyke$strict)) { NULL } else { c("strict", rep(NA, times = nrow(opdyke$strict))) }) }),
                               data.frame(c(NA, rownames(opdyke$config)), do.call("rbind", lapply(opdyke, function(y) rbind(NA, y))),
                                          row.names = NULL, fix.empty.names = FALSE), row.names = NULL, fix.empty.names = FALSE)

        }

        # Round
        opdyke[, -c(1L:2L)] <- sapply(opdyke[, -c(1L:2L)], round, digits = p.digits)

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Write Object ####

    write.object <- list(Summary = summary, Coverage = coverage, Itemstat = itemstat, Fit = fit, Param = param, Modind = modind, Score = score, Resid = resid, Opdyke = opdyke)

  #_____________________________________________________________________________
  #
  # Effect Size Measure of Measurement Non-Invariance, item.noninvar() ---------

  }, item.noninvar = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Check Inputs ####

    .check.input(m.character = list(write = c("summary", "dmacs", "bias")), envir = environment(), input.check = check)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## lavaan summary ####

    summary <- NULL
    if (isTRUE("summary" %in% write)) {

      # Extract result table
      summary <- write.object$summary

      # Column names
      colnames(summary) <- c(summary[1L, 1L], rep("", times = ncol(summary) - 1L))

      # Remove first row
      summary <- summary[-1L, ]

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## dMACS ####

    write.dmacs <- NULL
    if (isTRUE("dmacs" %in% write)) {

      #—————————————————————————————————————— #
      ### Extract Output ####

      write.dmacs <- write.object$noninvar$dmacs

      #—————————————————————————————————————— #
      ### Two Groups or Time Points ####

      if (isTRUE((is.null(dim(write.dmacs)) || is.data.frame(write.dmacs)) && class(write.dmacs) != "list")) {

        #...................
        #### One Factor ####

        if (isTRUE(!is.data.frame(write.dmacs))) {

          # Round
          write.dmacs <- round(write.dmacs, digits = digits)

          # Names and dMACS
          write.dmacs <- as.data.frame(matrix(write.dmacs, ncol = length(write.dmacs), dimnames = list(NULL, names(write.dmacs))))

        #...................
        #### More than One Factor ####

        } else {

          # Round
          write.dmacs <- round(write.dmacs, digits = digits)

          # Names and dMACS
          write.dmacs <- setNames(data.frame(names = rownames(write.dmacs), dMACS = write.dmacs), c("Var", names(write.object$noninvar$dmacs)))

        }

      #—————————————————————————————————————— #
      ### More than Two Groups or Time Points ####

      } else {

        #...................
        #### One Factor ####

        if (isTRUE(all(sapply(write.dmacs, function(y) is.null(dim(y)))))) {

          # Round
          write.dmacs <- lapply(write.dmacs, round, digits = digits)

          # Names and dMACS
          for (i in names(write.dmacs)) {

            write.dmacs[[i]] <- setNames(data.frame(if (isTRUE(!x$args$long)) { paste0("Reference Group ", x$args$ref, " vs. ", "Focal Group ", i) } else { paste0("Reference Time Points ", x$args$ref, " vs. ", "Focal Time Point ", i) },
                                                    matrix(write.dmacs[[i]], ncol = length(write.dmacs[[i]]))), nm = c("", names(write.object$noninvar$dmacs[[1L]])))

          }

          # Row bind
          write.dmacs <- do.call("rbind", write.dmacs)

        #...................
        #### More than One Factor ####

        } else {

          # Round
          write.dmacs <- lapply(write.dmacs, round, digits = digits)

          # Names and dMACS
          for (i in names(write.dmacs)) {

            write.dmacs[[i]] <- setNames(data.frame(if (isTRUE(!x$args$long)) { paste0("Reference Group ", x$args$ref, " vs. ", "Focal Group ", i) } else { paste0("Reference Time Points ", x$args$ref, " vs. ", "Focal Time Point ", i) },
                                                    rownames(write.dmacs[[i]]), write.dmacs[[i]]), nm = c("", "Var", names(write.object$noninvar$dmacs[[1L]])))

          }

          # Row bind
          write.dmacs <- do.call("rbind", write.dmacs)

          # Duplicated
          write.dmacs[duplicated(write.dmacs[, 1L]), 1L] <- ""

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bias ####

    write.bias <- NULL
    if (isTRUE("bias" %in% write)) {

      #—————————————————————————————————————— #
      ### Extract Output ####

      write.bias <- write.object$noninvar[-1L]

      #—————————————————————————————————————— #
      ### Two Groups or Time Points ####

      if (isTRUE(all(sapply(write.bias, function(y) !is.list(y) || (is.list(y) & is.data.frame(y)))))) {

        #...................
        #### One Factor ####

        if (isTRUE(is.null(dim(write.bias$m.diff)))) {

          # Round
          write.bias <- lapply(write.bias, function(y) if (isTRUE(!is.null(y))) { round(y, digits = digits) })

          # Format
          if (!isTRUE(x$args$ordered)) {

            write.bias <- setNames(as.data.frame(rbind(c("DMean", write.bias$m.diff), c("DVar", write.bias$v.diff))), nm = c("D", "f"))

          } else {

            write.bias <- setNames(as.data.frame(cbind("DMean", write.bias$m.diff)), nm = c("D", "f"))

          }

        #...................
        #### More than One Factor ####

        } else {

          # Round
          write.bias <- lapply(write.bias, function(y) if (isTRUE(!is.null(y))) { sapply(y, function(z) round(z, digits = digits)) })

          # Format
          if (!isTRUE(x$args$ordered)) {

            write.bias <- rbind(data.frame(D = "DMean", matrix(write.bias$m.diff, ncol = length(write.bias$m.diff), dimnames = list(NULL, names(write.bias$m.diff)))),
                                data.frame(D = "VMean", matrix(write.bias$v.diff, ncol = length(write.bias$v.diff), dimnames = list(NULL, names(write.bias$v.diff)))))

          } else {

            write.bias <- data.frame(D = "DMean", matrix(write.bias$m.diff, ncol = length(write.bias$m.diff), dimnames = list(NULL, names(write.bias$m.diff))))

          }

        }

      #—————————————————————————————————————— #
      ### More than Two Groups or Time Points ####

      } else {

        #...................
        #### One Factor ####

        if (isTRUE(all(sapply(write.bias, function(y) sapply(y, length)) <= 1L))) {

          # Round
          write.bias.temp <- lapply(write.bias, function(y) lapply(y, function(z) if (isTRUE(!is.null(z))) { round(z, digits = digits) }))

          # Format
          write.bias <- list()
          if (!isTRUE(x$args$ordered)) {

            for(i in names(write.bias.temp[[1L]])) { write.bias[[i]] <- setNames(data.frame(if (isTRUE(!x$args$long)) { paste0("Reference Group ", x$args$ref, " vs. ", "Focal Group ", i) } else { paste0("Reference Time Points ", x$args$ref, " vs. ", "Focal Time Point ", i) }, c("DMean", "DVar"), unlist(do.call("rbind", lapply(write.bias.temp, function(y) y[i])))), nm = c("", "D", "f"))  }

          } else {

            for(i in names(write.bias.temp[[1L]])) { write.bias[[i]] <- setNames(data.frame(if (isTRUE(!x$args$long)) { paste0("Reference Group ", x$args$ref, " vs. ", "Focal Group ", i) } else { paste0("Reference Time Points ", x$args$ref, " vs. ", "Focal Time Point ", i) }, "DMean", unlist(do.call("rbind", lapply(write.bias.temp, function(y) y[i])))), nm = c("", "D", "f"))  }

          }

          # Row bind
          write.bias <- do.call("rbind", write.bias)

          # Duplicated
          write.bias[duplicated(write.bias[, 1L]), 1L] <- ""

        #...................
        #### More than One Factor ####

        } else {

          # Round
          write.bias.temp <- lapply(write.bias, function(y) lapply(y, function(z) if (isTRUE(!is.null(z))) { round(z, digits = digits) }))

          # Format
          write.bias <- list()
          if (!isTRUE(x$args$ordered)) {

            for(i in names(write.bias.temp[[1L]])) {

              write.bias[[i]] <- setNames(data.frame(i, c("DMean", "DVar"), do.call("rbind", lapply(write.bias.temp, function(y) y[[i]]))), nm = c(ifelse(!x$args$long, "Focal Group", "Focal Time Point"), "D", names(write.bias.temp$m.diff[[i]])))

            }

          } else {

            for(i in names(write.bias.temp[[1L]])) { write.bias[[i]] <- setNames(data.frame(i, "DMean", do.call("rbind", lapply(write.bias.temp, function(y) y[[i]]))), nm = c(ifelse(!x$args$long, "Focal Group", "Focal Time Point"), "D", names(write.bias.temp$m.diff[[i]])))  }

          }

          # Row bind
          write.bias <- do.call("rbind", write.bias)

          # Duplicated
          write.bias[duplicated(write.bias[, 1L]), 1L] <- ""

        }

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Write Object ####

    write.object <- list(Summary = summary, dMACS = write.dmacs, Bias = write.bias)

  #_____________________________________________________________________________
  #
  # Coefficient Omega, item.omega() --------------------------------------------
  }, item.omega = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Omega ####

    omega <- NULL
    if (isTRUE(!is.null(write.object$omega))) {

      # Extract result
      omega <- write.object$omega

      #—————————————————————————————————————— #
      ### Round ####

      omega[, c("omega", "low", "upp")] <- round(omega[, c("omega", "low", "upp")], digits = r.digits)

      #—————————————————————————————————————— #
      ### Variable Names ####

      colnames(omega) <- c("n", "nNA", "nItems", "Alpha", "Low", "Upp")

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Item Statistics ####

    itemstat <- NULL
    if (isTRUE(!is.null(write.object$itemstat))) {

      # Extract result
      itemstat <- write.object$itemstat

      #—————————————————————————————————————— #
      ### Round ####

      itemstat[, c("m", "sd", "min", "max")] <- round(itemstat[, c("m", "sd", "min", "max")], digits = digits)

      itemstat[, c("std.ld", "omega", "d.omega")] <- round(itemstat[, c("std.ld", "omega", "d.omega")], digits = r.digits)

      #—————————————————————————————————————— #
      ### Variable Names ####

      colnames(itemstat) <- c("Item", "n", "nNA", "%NA", "M", "SD", "Min", "Max", "Std.Ld", "Omega", "dOmega")

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Write Object ####

    write.object <- list(Omega = omega, Itemstast = itemstat)

  #_____________________________________________________________________________
  #
  # Item Analysis, item.stats() ------------------------------------------------
  }, item.stats = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Round ####

    if (isTRUE(any(write.object$pNA != 0L))) { write.object$pNA <- round(write.object$pNA, digits = digits) }

    if (isTRUE("p" %in% names(write.object))) { write.object$p <- round(write.object$p, digits = r.digits) }
    if (isTRUE("m" %in% names(write.object))) { write.object$m <- round(write.object$m, digits = digits) }
    if (isTRUE("med" %in% names(write.object))) { write.object$med <- round(write.object$med, digits = digits) }
    if (isTRUE("min" %in% names(write.object))) { write.object$min <- round(write.object$min, digits = digits) }
    if (isTRUE("max" %in% names(write.object))) { write.object$max <- round(write.object$max, digits = digits) }

    write.object$sd <- round(write.object$sd, digits = digits)

    write.object$r <- round(write.object$r, digits = r.digits)
    write.object$low <- round(write.object$low, digits = r.digits)
    write.object$upp <- round(write.object$upp, digits = r.digits)
    write.object$alpha <- round(write.object$alpha, digits = r.digits)
    write.object$d.alpha <- round(write.object$d.alpha, digits = r.digits)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Variable Names ####

    names(write.object) <- switch(x$dtype,
                                  "dicho" = { c("Item", "n", "nNA", "%NA", "n0", "n1", "P", "SD", "r", "Low", "Upp", "Alpha", "dAlpha") },
                                  "poly"  = { c("Item", "n", "nNA", "%NA", colnames(write.object)[5L:(which(colnames(write.object) == "m") - 1L)], "M", "Med", "SD", "Min", "Max", "r", "Low", "Upp", "Alpha", "dAlpha") },
                                  "cont"  = { c("Item", "n", "nNA", "%NA", "M", "SD", "Min", "Max", "r", "Low", "Upp", "Alpha", "dAlpha") })


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Write object ####

    write.object <- list(Itemstats = write.object)

  #_____________________________________________________________________________
  #
  # Summary Measures, Convergence and Efficiency Diagnostics, mplus.bayes() ----
  }, mplus.bayes = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Round ####

    # digits
    print.round <- c("m", "med", "map", "sd", "mad", "skew", "kurt", "eti.low", "eti.upp", "hdi.low", "hdi.upp")
    write.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(write.object[, y]), round(write.object[, y], digits = digits), NA))

    # r.digits
    write.object[, "rhat"] <- ifelse(!is.na(write.object[, "rhat"]), round(write.object[, "rhat"], digits = r.digits), NA)

    # ess.digits
    write.object[, "b.ess"] <- ifelse(!is.na(write.object[, "b.ess"]), round(write.object[, "b.ess"], digits = ess.digits), NA)
    write.object[, "t.ess"] <- ifelse(!is.na(write.object[, "t.ess"]), round(write.object[, "t.ess"], digits = ess.digits), NA)

    # mcse.digits
    write.object[, "b.mcse"] <- ifelse(!is.na(write.object[, "b.mcse"]), round(write.object[, "b.mcse"], digits = mcse.digits), NA)
    write.object[, "t.mcse"] <- ifelse(!is.na(write.object[, "t.mcse"]), round(write.object[, "t.mcse"], digits = mcse.digits), NA)

    # p.digits
    write.object[, "pd"] <- ifelse(!is.na(write.object[, "pd"]), round(write.object[, "pd"], digits = p.digits), NA)
    write.object[, "rope"] <- ifelse(!is.na(write.object[, "rope"]), round(write.object[, "rope"], digits = p.digits), NA)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Variable Names ####

    colnames(write.object) <- c("Parameter", "M", "Med", "MAP", "SD", "MAD", "Skew", "Kurt", "ETI.Low", "ETI.Upp", "HDI.Low", "HDI.Upp", "R-hat", "B.ESS", "T.ESS", "B.MCSE", "T.MCSE", "pd", "ROPE")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Select Statistical Measures and Add Parameters ####

    # Print statistics
    print <- misty::rec(write, spec = "'m' = 'M'; 'med' = 'Med'; 'map' = 'MAP'; 'sd' = 'SD'; 'mad' = 'MAD'; 'skew' = 'Skew'; 'kurt' = 'Kurt'; 'rhat' = 'R-hat'; 'b.ess' = 'B.ESS'; 't.ess' = 'T.ESS'; 'b.mcse' = 'B.MCSE'; 't.mcse' = 'T.MCSE'; 'rope' = 'ROPE'")

    if (isTRUE("eti" %in% print)) { print <- c(print, c("ETI.Low", "ETI.Upp")) }
    if (isTRUE("hdi" %in% print)) { print <- c(print, c("HDI.Low", "HDI.Upp")) }

    # Sort
    print <- intersect(c("M", "Med", "MAP", "SD", "MAD", "Skew", "Kurt", "ETI.Low", "ETI.Upp", "HDI.Low", "HDI.Upp", "R-hat", "B.ESS", "T.ESS", "B.MCSE", "T.MCSE"), print)

    # Select
    write.object <- data.frame(Parameter = write.object[, "Parameter"], write.object[, print, drop = FALSE], stringsAsFactors = FALSE, check.names = FALSE)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Note ####

    note <- NULL

    # R-hat
    if (isTRUE("R-hat" %in% print)) {

      if (isTRUE(x$args$fold)) {

        note <- rbind(note, data.frame("Maximum of Rank-Normalized (Folded-)Split R-hat", fix.empty.names = FALSE))

      } else {

        if (isTRUE(x$args$rank)) {

          if (isTRUE(x$args$split)) {

            note <- rbind(note, data.frame("Rank-Normalizsed Split R-hat", fix.empty.names = FALSE))

          } else {

            note <- rbind(note, data.frame("Rank-Normalizsed R-hat", fix.empty.names = FALSE))

          }

        } else {

          if (isTRUE(x$args$split)) {

            note <- rbind(note, data.frame("Traditional Split R-hat", fix.empty.names = FALSE))

          } else {

            note <- rbind(note, data.frame("Traditional R-hat", fix.empty.names = FALSE))

          }

        }

      }

    }

    # ROPE
    if (isTRUE(!is.null(x$args$rope))) {

      if (isTRUE("ROPE" %in% print)) {

        note <- rbind(note, data.frame(paste0("Region of Practical Equivalence (ROPE): [", x$args$rope[1L], ", ", x$args$rope[2L], "]"), fix.empty.names = FALSE))

      } else {

        note <- rbind(note, data.frame(paste0("Region of Practical Equivalence (ROPE): [", x$args$rope[1L], ", ", x$args$rope[2L], "]"), fix.empty.names = FALSE))

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Write Object ####

    if (isTRUE(!is.null(note))) { write.object <- list(Summary = write.object, Note = note) }

  #_____________________________________________________________________________
  #
  #  Multilevel Coefficient Alpha, multilevel.alpha() --------------------------
  }, multilevel.alpha = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Alpha ####

    write.alpha <- NULL
    if (isTRUE("alpha" %in% write)) {

      #—————————————————————————————————————— #
      ### Extract Result Table ####

      write.alpha <- write.object$alpha

      #—————————————————————————————————————— #
      ### Round ####

      write.alpha[, setdiff(colnames(write.alpha), c("type", "items"))] <- sapply(write.alpha[, setdiff(colnames(write.alpha), c("type", "items"))], round, digits = digits)

      #—————————————————————————————————————— #
      ### Column Names ####

      if (isTRUE(x$args$se != "none")) {

        colnames(write.alpha) <- c("Type", "Items", "Omega", "Low", "Upp")

      } else {

        colnames(write.alpha) <- c("Type", "Items", "Omega")

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Item Statistics ####

    write.item <- NULL
    if (isTRUE("item" %in% write)) {

      #—————————————————————————————————————— #
      ### Extract Result Table ####

      write.item <- write.object$item

      #—————————————————————————————————————— #
      ### Round ####

      # Variables to round
      write.round <- c("pNA", "m", "sd", "min", "max", "skew", "kurt", "ICC", "wstd.ld", "bstd.ld")

      write.item[, write.round] <- sapply(write.item[, write.round], round, digits = digits)

      #—————————————————————————————————————— #
      ### Column Names ####

      colnames(write.item) <- c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max",  "Skew", "Kurt", "ICC(1)", "WStd.ld", "BStd.ld")

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Write Object ####

    write.object <- list(Alpha = write.alpha, Itemstat = write.item)

  #_____________________________________________________________________________
  #
  # Multilevel Confirmatory Factor Analysis, multilevel.cfa() ------------------

  }, multilevel.cfa = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## lavaan Summary ####

    summary <- NULL
    if (isTRUE("summary" %in% write)) {

      # Column names
      colnames(write.object$summary) <- c(write.object$summary[1L, 1L], "", "")

      summary <- write.object$summary[-1L, ]

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Covariance Coverage ####

    coverage <- NULL
    if (isTRUE("coverage" %in% write)) {

      # Round
      write.object$coverage <- sapply(data.frame(write.object$coverage), round, digits = digits)

      # Add variable names in the rows
      coverage <- data.frame(colnames(write.object$coverage), write.object$coverage,
                             row.names = NULL, check.rows = FALSE,
                             check.names = FALSE, fix.empty.names = FALSE)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Univariate Sample Statistics ####

    descript <- NULL
    if (isTRUE("descript" %in% write)) {

      itemstat <- write.object$descript

      # Round
      itemstat[, -1L] <- sapply(itemstat[, -1L], round, digits = digits)

      colnames(itemstat) <- c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "Skew", "Kurt", "ICC(1)")

    }

    #—————————————————————————————————————— #
    ### Model Fit ####

    fit <- NULL
    if (isTRUE("fit" %in% write)) {

      fit <- write.object$fit

      # Round
      fit[, -1L] <- sapply(fit[, -1L], round, digits = digits)

      # Estimator = "ML"
      if (isTRUE(ncol(write.object$fit) == 2L)) {

        colnames(fit) <- c("", "Standard")

      } else {

        colnames(fit) <- c("", "Standard", "Scaled", "Robust")

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Parameter Estimates ####

    param <- NULL
    if (isTRUE("est" %in% write)) {

      param <- rbind(data.frame(Level = "Within", write.object$param$within),
                     data.frame(Level = "Between", write.object$param$between))

      # Round
      param[, setdiff(colnames(param), c("Level", "param", "lhs", "op", "rhs", "pvalue"))] <- sapply(param[, setdiff(colnames(param), c("Level", "param", "lhs", "op", "rhs", "pvalue"))], round, digits = digits)
      if (isTRUE("pvalue" %in% colnames(param))) { param[, "pvalue"] <- sapply(param[, "pvalue"], round, digits = p.digits) }

      # Column names
      if (isTRUE(x$args$se  != "none")) {

        colnames(param) <- c("Parameter", "Variable", "lhs", "op", "rhs", "Estimate", "SE", "z", "pvalue", "StdYX")

      } else {

        colnames(param) <- c("Parameter", "Variable", "lhs", "op", "rhs", "Estimate", "StdYX")

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Modification Indices ####

    modind <- score <- NULL
    if (isTRUE("modind" %in% write)) {

      if (isTRUE(nrow(write.object$modind$within) == 0L)) {

        write.object$modind$within <- data.frame(matrix(NA, ncol = 6L, dimnames = list(NULL, names(write.object$modind$within))))

      }

      if (isTRUE(nrow(write.object$modind$between) == 0L)) {

        write.object$modind$between <- data.frame(matrix(NA, ncol = 6L, dimnames = list(NULL, names(write.object$modind$between))))

      }

      modind <- rbind(data.frame(Level = "Within", write.object$modind$within), data.frame(Level = "Between", write.object$modind$between))

      # Round
      modind[, -c(1L:4L)] <- sapply(modind[, -c(1L:4L)], round, digits = digits)

      colnames(modind) <- c("Level", "lhs", "op", "rhs", "MI", "EPC", "STDYX EPC")

      #—————————————————————————————————————— #
      ### Modification Indices for Parameter Constaints ####

      if (isTRUE(!is.null(write.object$score))) {

        # Extract result table
        score <- write.object$score

        # Round
        score[, c("mi", "lhs.epc", "rhs.epc", "lhs.stdyx", "rhs.stdyx")] <- sapply(score[, c("mi", "lhs.epc", "rhs.epc", "lhs.stdyx", "rhs.stdyx")], round, digits = digits)
        score[, "pvalue"] <- round(score[, "pvalue"], digits = p.digits)

        # Column names
        colnames(score) <- c("Label", "lhs", "op", "rhs", "MI", "df", "pvalue", "lhs.EPC", "rhs.EPC", "lhs.StdYX", "rhs.StdYX")

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Residual Correlation Matrix ####

    resid <- NULL
    if (isTRUE("resid" %in% write)) {

      if (isTRUE(!is.null(write.object$resid))) {

        # Extract result table
        resid <- write.object$resid

        # Combine Within and Between level
        resid <- data.frame(c("Within", rep("", times = nrow(resid[[1L]])), "Between", rep("", times = nrow(resid[[1L]]))),
                            c("",  rownames(resid[[1L]]), "", rownames(resid[[2L]])),
                              do.call("rbind", lapply(resid, function(z) rbind(NA, z))), row.names = NULL, fix.empty.names = FALSE)

        # Round
        resid[, -c(1L:2L)] <- sapply(resid[, -c(1L:2L)], round, digits = p.digits)

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Relative Opdyke Distribution Percentile Matrix ####

    opdyke <- NULL
    if (isTRUE("opdyke" %in% write)) {

      if (isTRUE(!is.null(write.object$opdyke))) {

        # Extract result table
        opdyke <- write.object$opdyke

        # Lower  Triangular
        opdyke[upper.tri(opdyke)] <- NA

        # Combine Within and Between level
        opdyke <- data.frame(c("Within", rep("", times = nrow(opdyke[[1L]])), "Between", rep("", times = nrow(opdyke[[1L]]))),
                             c("",  rownames(opdyke[[1L]]), "", rownames(opdyke[[2L]])),
                             do.call("rbind", lapply(opdyke, function(z) rbind(NA, z))), row.names = NULL, fix.empty.names = FALSE)

        # Round
        opdyke[, -c(1L:2L)] <- sapply(opdyke[, -c(1L:2L)], round, digits = p.digits)

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Write Object ####

    write.object <- list(Summary = summary, Coverage = coverage, Descript = itemstat,
                         Fit = fit, Param = param, Modind = modind, Score = score, Resid = resid, Opdyke = opdyke)

  #_____________________________________________________________________________
  #
  # Within- and Between-Group Correlation Matrix, multilevel.cor() -------------

  }, multilevel.cor = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Split Results ####

    if (isTRUE(x$args$split)) {

      #### Round
      write.object$with.cor <- sapply(data.frame(write.object$with.cor), round, digits = digits)
      write.object$with.se <- sapply(data.frame(write.object$with.se), round, digits = digits)
      write.object$with.stat <- sapply(data.frame(write.object$with.stat), round, digits = digits)
      write.object$with.p <- sapply(data.frame(write.object$with.p), round, digits = p.digits)

      write.object$betw.cor <- sapply(data.frame(write.object$betw.cor), round, digits = digits)
      write.object$betw.se <- sapply(data.frame(write.object$betw.se), round, digits = digits)
      write.object$betw.stat <- sapply(data.frame(write.object$betw.stat), round, digits = digits)
      write.object$betw.p <- sapply(data.frame(write.object$betw.p), round, digits = p.digits)

      #### Lower and/or upper triangular
      if (isTRUE(x$args$tri == "lower")) {

        write.object$with.cor[upper.tri(write.object$with.cor)] <- NA
        write.object$with.se[upper.tri(write.object$with.se)] <- NA
        write.object$with.stat[upper.tri(write.object$with.stat)] <- NA
        write.object$with.p[upper.tri(write.object$with.p)] <- NA

        write.object$betw.cor[upper.tri(write.object$betw.cor)] <- NA
        write.object$betw.se[upper.tri(write.object$betw.se)] <- NA
        write.object$betw.stat[upper.tri(write.object$betw.stat)] <- NA
        write.object$betw.p[upper.tri(write.object$betw.p)] <- NA

      }

      if (isTRUE(x$args$tri == "upper")) {

        write.object$with.cor[lower.tri(write.object$with.cor)] <- NA
        write.object$with.se[lower.tri(write.object$with.se)] <- NA
        write.object$with.stat[lower.tri(write.object$with.stat)] <- NA
        write.object$with.p[lower.tri(write.object$with.p)] <- NA

        write.object$betw.cor[lower.tri(write.object$betw.cor)] <- NA
        write.object$betw.se[lower.tri(write.object$betw.se)] <- NA
        write.object$betw.stat[lower.tri(write.object$betw.stat)] <- NA
        write.object$betw.p[lower.tri(write.object$betw.p)] <- NA

      }

      write.object <- list(Summary = write.object$summary,
                           With.cor = write.object$with.cor, with.se = write.object$with.se,
                           With.stat = write.object$with.stat, with.p = write.object$with.p,
                           Betw.cor = write.object$betw.cor, betw.se = write.object$betw.se,
                           Betw.stat = write.object$betw.stat, betw.p = write.object$betw.p)

      #### Add 'Lower triangular: Within-Group, Upper triangular: Between-Group
      write.object$Summary <- data.frame(rbind(write.object$Summary,
                                               c(NA, NA, NA),
                                               c("Lower triangular: Within-Group, Upper triangular: Between-Group", NA, NA)),
                                         row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Combined Results ####

    } else {

      #### Round
      write.object$wb.cor <- sapply(data.frame(write.object$wb.cor), round, digits = digits)
      write.object$wb.se <- sapply(data.frame(write.object$wb.se), round, digits = digits)
      write.object$wb.stat <- sapply(data.frame(write.object$wb.stat), round, digits = digits)
      write.object$wb.p <- sapply(data.frame(write.object$wb.p), round, digits = p.digits)

      write.object <- list(Summary = write.object$summary,
                           Cor = write.object$wb.cor, se = write.object$wb.se,
                           Stat = write.object$wb.stat, p = write.object$wb.p)

      #### Print
      if (isTRUE(!"summary" %in% write)) { write.object$summary <- NULL }
      if (isTRUE(!"cor" %in% write)) { write.object$cor <- NULL }
      if (isTRUE(!"se" %in% write)) { write.object$se <- NULL }
      if (isTRUE(!"stat" %in% write)) { write.object$stat <- NULL }
      if (isTRUE(!"p" %in% write)) { write.object$p <- NULL }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Add Variable Names in the Rows ####

    write.object[-1L] <- lapply(write.object[-1L], function(y) data.frame(colnames(y), y,
                                                                          row.names = NULL, check.rows = FALSE,
                                                                          check.names = FALSE, fix.empty.names = FALSE))

  #_____________________________________________________________________________
  #
  # Multilevel Descriptive Statistics, multilevel.descript() -------------------

  }, multilevel.descript = {

    write.object <- data.frame(c("Level 1", "No. of cases", "No. of missing values", "", "Variance Within", "SD Within", "",
                                 "Level 2", "No. of clusters", "Average cluster size", "SD cluster size", "Min cluster size", "Max cluster size", "", "Mean", "Variance Between", "SD Between", "ICC(1)", "ICC(2)", "",
                                 "Level 3", "No. of clusters", "Average cluster size", "SD cluster size", "Min cluster size", "Max cluster size", "", "Mean", "Variance Between", "SD Between", "ICC(1)", "ICC(2)", "",
                                 "Design effect", "Design effect sqrt", "Effective sample size"),
                               rbind(NA, write.object$no.obs, write.object$no.miss, NA, write.object$var.r, write.object$sd.r, NA,
                                     NA, write.object$no.cluster.l2, write.object$m.cluster.size.l2, write.object$sd.cluster.size.l2, write.object$min.cluster.size.l2, write.object$max.cluster.size.l2, NA, write.object$mean.x, write.object$var.u, write.object$sd.u, write.object$icc1.l2, write.object$icc2.l2, NA,
                                     NA, write.object$no.cluster.l3, write.object$m.cluster.size.l3, write.object$sd.cluster.size.l3, write.object$min.cluster.size.l3, write.object$max.cluster.size.l3, NA, write.object$mean.x, write.object$var.v, write.object$sd.v, write.object$icc1.l3, write.object$icc2.l3, NA,
                                     write.object$deff, write.object$deff.sqrt, write.object$n.effect), fix.empty.names = FALSE, stringsAsFactors = FALSE)


    #### Round
    for (i in c(5L:6L, 10L:11L, 15L:17L, 23L:24L, 28L:30L, 34L:36L)) { write.object[i, 2L:ncol(write.object)] <- round(write.object[i, 2L:ncol(write.object)], digits = digits) }

    for (i in c(18L:19L, 31L:32L)) { write.object[i, 2L:ncol(write.object)] <- round(write.object[i, 2L:ncol(write.object)], digits = icc.digits) }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Select Rows ####

    #—————————————————————————————————————— #
    ### One Cluster ####

    if (isTRUE(x$no.clust == "one")) {

      write.object <- write.object[-c(20L:32L), ]

      # All Between variables
      if (isTRUE(all(is.na(write.object[18L, -1])))) {

        write.object <- write.object[c(8L:9L, 14L, 15L:17L), ]

      }

    #—————————————————————————————————————— #
    ### Two ClusterS ####

    } else {

      write.object <- write.object[-15L, ]

      # All Between variables
      if (isTRUE(all(is.na(write.object[5L, -1])))) {

        # Only Level 3 Variables
        if (isTRUE(all(is.na(write.object[16L, -1])))) {

          write.object <- write.object[c(20L:21L, 26L:29L), ]

        # Level 2 Variables
        } else {

          write.object <- write.object[c(8L:9L, 14L:16L, 19L:35L), ]

        }

      }

    }

    # Variance and/or SD
    if (isTRUE(!"var" %in% write)) { write.object <- write.object[-grep("Variance", write.object[, 1L]), ] }
    if (isTRUE(!"sd" %in% write)) { write.object <- write.object[-grep("SD", write.object[, 1L]), ] }

  #_____________________________________________________________________________
  #
  # Simultaneous and Level-Specific Multilevel Model Fit Information, multievel.fit() ----

  }, multilevel.fit = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## lavaan Summary ####

    summary <- NULL
    if (isTRUE("summary" %in% write)) {

      # Column names
      colnames(write.object$summary) <- c(write.object$summary[1L, 1L], "", "")

      summary <- write.object$summary[-1L, ]

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Model Fit ####

    fit <- NULL
    if (isTRUE("fit" %in% write)) {

      fit <- write.object$fit

      # Round
      fit[, -1L] <- round(fit[, -1L], digits = digits)

      # Estimator = "ML"
      if (isTRUE(ncol(fit) == 2L)) {

        colnames(fit) <- c("", "Standard")

      # Estimator = "MLR"
      } else {

        colnames(fit) <- c("", "Standard", "Scaled", "Robust")

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Write Object ####

    write.object <- list(Summary = summary, Fit = fit)

  #_____________________________________________________________________________
  #
  # Cross-Level Measurement Invariance Evaluation, multievel.invar() ----

  }, multilevel.invar = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## lavaan summary ####

    summary <- NULL
    if (isTRUE("summary" %in% write)) {

      # Column names
      colnames(write.object$summary) <- c(write.object$summary[1L, 1L], "", "")

      summary <- write.object$summary[-1L, ]

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Covariance coverage ####

    coverage <- NULL
    if (isTRUE("coverage" %in% write)) {

      # Round
      write.object$coverage <- sapply(data.frame(write.object$coverage), round, digits = digits)

      # Add variable names in the rows
      coverage <- data.frame(colnames(write.object$coverage), write.object$coverage,
                             row.names = NULL, check.rows = FALSE,
                             check.names = FALSE, fix.empty.names = FALSE)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Univariate Sample Statistics ####

    descript <- NULL
    if (isTRUE("descript" %in% write)) {

      itemstat <- write.object$descript

      # Round
      itemstat[, -1L] <- sapply(itemstat[, -1L], round, digits = digits)

      colnames(itemstat) <- c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "Skew", "Kurt", "ICC(1)")

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Model fit ####

    fit <- NULL
    if (isTRUE("fit" %in% write)) {

      # Extract result table
      fit <- write.object$fit

      # Remove NULL entries
      fit <- fit[!sapply(fit, is.null)]

      #### Standard fit indices
      if (isTRUE(x$args$estimator %in% c("ML", "MLF", "GLS", "WLS", "DWLS", "ULS", "PML"))) {

        # Combine data frames
        fit <- data.frame(c("Standard", rep(NA, times = nrow(fit$stand))),
                          rbind(NA, fit$stand),
                          row.names = NULL, fix.empty.names = FALSE)

      #### Standard, scaled, and robust fit indices
      } else {

        # Combine data frames
        fit <- data.frame(c("Standard", rep(NA, times = nrow(fit$stand)), "Scaled", rep(NA, times = nrow(fit$scaled)), "Robust", rep(NA, times = nrow(fit$robust))),
                          do.call("rbind", lapply(fit, function(y) rbind(NA, y))),
                          row.names = NULL, fix.empty.names = FALSE)

      }

      # Round
      fit[which(!fit[, 2L] %in% c("P-value", "P-value RMSEA <= 0.05")), c(3L:ncol(fit))] <- sapply(fit[which(!fit[, 2L] %in% c("P-value", "P-value RMSEA <= 0.05")), c(3L:ncol(fit))], round, digits = digits)
      fit[which(fit[, 2L] %in% c("P-value", "P-value RMSEA <= 0.05")), c(3L:ncol(fit))] <- sapply(fit[which(fit[, 2L] %in% c("P-value", "P-value RMSEA <= 0.05")), c(3L:ncol(fit))], round, digits = p.digits)

      # Column names
      switch(x$args$invar,
             config = { colnames(fit) <- c("", "", "Config") },
             metric = { colnames(fit) <- c("", "", "Config", "Metric", "dMetric") },
             scalar = { colnames(fit) <- c("", "", "Config", "Metric", "Scalar", "dMetric", "dScalar") })

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Parameter estimates ####

    param <- NULL
    if (isTRUE("est" %in% write)) {

      # Extract result table
      param <- write.object$param

      # Remove NULL entries
      param <- param[!sapply(param, is.null)]

      # Combine data frames
      param <- lapply(lapply(param, function(y) do.call("rbind", lapply(y, function(z) rbind(NA, z)))), function(q) data.frame(c("Within", rep(NA, times = nrow(q) / 2L - 1L), "Between", rep(NA, times = nrow(q) / 2L - 1L)), q, row.names = NULL, fix.empty.names = FALSE))

      # Combine data frames
      param <- data.frame(switch(x$args$invar,
                                 config = { c("Config", rep(NA, times = nrow(param$config))) },
                                 metric = { c("Config", rep(NA, times = nrow(param$config)), "Metric", rep(NA, times = nrow(param$metric))) },
                                 scalar = { c("Config", rep(NA, times = nrow(param$config)), "Metric", rep(NA, times = nrow(param$metric)), "Scalar", rep(NA, times = nrow(param$scalar))) }),
                          do.call("rbind", lapply(param, function(y) rbind(NA, y))),
                          row.names = NULL, fix.empty.names = FALSE)

      # Round
      param[, c("est", "se", "z", "stdyx")] <- sapply(param[, c("est", "se", "z", "stdyx")], round, digits = digits)
      param[, "pvalue"] <- round(param[, "pvalue"], digits = p.digits)

      # Column names
      colnames(param) <- c("", "Parameter", "lhs", "op", "rhs", "label", "Estimate", "SE", "z", "pvalue", "StdYX")

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Modification Indices ####

    modind <- NULL
    if (isTRUE("modind" %in% write)) {

      if (isTRUE(any(!sapply(write.object$modind, is.null)))) {

        # Extract result table
        modind <- write.object$modind

        # Remove NULL entries
        modind <- modind[!sapply(modind, is.null)]

        # Combine data frames
        modind <- lapply(lapply(modind, function(y) do.call("rbind", lapply(y, function(z) rbind(NA, z)))), function(q) data.frame(c("Within", rep(NA, times = nrow(q) / 2L - 1L), "Between", rep(NA, times = nrow(q) / 2L - 1L)), q, row.names = NULL, fix.empty.names = FALSE))

        # Combine data frames
        modind <- data.frame(switch(x$args$invar,
                                    config = {   if (is.null(modind$config)) { NULL } else { c("Config", rep(NA, times = nrow(modind$config))) } },
                                    metric = { c(if (is.null(modind$config)) { NULL } else { c("Config", rep(NA, times = nrow(modind$config))) },
                                                 if (is.null(modind$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(modind$metric))) }) },
                                    scalar = { c(if (is.null(modind$config)) { NULL } else { c("Config", rep(NA, times = nrow(modind$config))) },
                                                 if (is.null(modind$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(modind$metric))) },
                                                 if (is.null(modind$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(modind$scalar))) }) }),
                             do.call("rbind", lapply(modind, function(y) rbind(NA, y))),
                             row.names = NULL, fix.empty.names = FALSE)

        # Round
        modind[, c("mi", "epc", "stdyx")] <- sapply(modind[, c("mi", "epc", "stdyx")], round, digits = digits)

        # Column names
        colnames(modind) <- c("", "lhs", "op", "rhs", "MI", "EPC", "StdYX")

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Residual Correlation Matrix ####

    resid <- NULL
    if (isTRUE("resid" %in% write)) {

      if (isTRUE(any(!sapply(write.object$resid, is.null)))) {

        # Extract result table
        resid <- write.object$resid

        # Remove NULL entries
        resid <- resid[!sapply(resid, is.null)]

        # Combine data frames
        resid <- lapply(lapply(resid, function(y) do.call("rbind", lapply(y, function(z) rbind(NA, z)))), function(q) data.frame(c("Within", rep(NA, times = nrow(q) / 2L - 1L), "Between", rep(NA, times = nrow(q) / 2L - 1L)),  c(NA, rownames(resid[[1]]$within)), q, row.names = NULL, fix.empty.names = FALSE))

        resid <- data.frame(switch(x$args$invar,
                                   config = {   if (is.null(resid$config)) { NULL } else { c("Config", rep(NA, times = nrow(resid$config))) } },
                                   metric = { c(if (is.null(resid$config)) { NULL } else { c("Config", rep(NA, times = nrow(resid$config))) },
                                                if (is.null(resid$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(resid$metric))) }) },
                                   scalar = { c(if (is.null(resid$config)) { NULL } else { c("Config", rep(NA, times = nrow(resid$config))) },
                                                if (is.null(resid$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(resid$metric))) },
                                                if (is.null(resid$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(resid$scalar))) }) }),
                            data.frame(do.call("rbind", lapply(resid, function(y) rbind(NA, y))),
                                       row.names = NULL, fix.empty.names = FALSE), row.names = NULL, fix.empty.names = FALSE)

        # Round
        resid[, -c(1L:3L)] <- sapply(resid[, -c(1L:3L)], round, digits = p.digits)

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Relative Opdyke Distribution Percentile Matrix ####

    opdyke <- NULL
    if (isTRUE("opdyke" %in% write)) {

      if (isTRUE(!is.null(write.object$opdyke))) {

        # Extract result table
        opdyke <- write.object$opdyke

        # Remove NULL entries
        opdyke <- opdyke[!sapply(opdyke, is.null)]

        # Lower  Triangular
        opdyke[upper.tri(opdyke)] <- NA

        # Combine data frames
        opdyke <- lapply(lapply(opdyke, function(y) do.call("rbind", lapply(y, function(z) rbind(NA, z)))), function(q) data.frame(c("Within", rep(NA, times = nrow(q) / 2L - 1L), "Between", rep(NA, times = nrow(q) / 2L - 1L)),  c(NA, rownames(opdyke[[1]]$within)), q, row.names = NULL, fix.empty.names = FALSE))

        opdyke <- data.frame(switch(x$args$invar,
                                   config = {   if (is.null(opdyke$config)) { NULL } else { c("Config", rep(NA, times = nrow(opdyke$config))) } },
                                   metric = { c(if (is.null(opdyke$config)) { NULL } else { c("Config", rep(NA, times = nrow(opdyke$config))) },
                                                if (is.null(opdyke$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(opdyke$metric))) }) },
                                   scalar = { c(if (is.null(opdyke$config)) { NULL } else { c("Config", rep(NA, times = nrow(opdyke$config))) },
                                                if (is.null(opdyke$metric)) { NULL } else { c("Metric", rep(NA, times = nrow(opdyke$metric))) },
                                                if (is.null(opdyke$scalar)) { NULL } else { c("Scalar", rep(NA, times = nrow(opdyke$scalar))) }) }),
                            data.frame(do.call("rbind", lapply(opdyke, function(y) rbind(NA, y))),
                                       row.names = NULL, fix.empty.names = FALSE), row.names = NULL, fix.empty.names = FALSE)

        # Round
        opdyke[, -c(1L:3L)] <- sapply(opdyke[, -c(1L:3L)], round, digits = p.digits)

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Write Object ####

    write.object <- list(Summary = summary, Coverage = coverage, Descript = itemstat,
                         Fit = fit, Param = param, Modind = modind, Resid = resid, Opdyke = opdyke)

  #_____________________________________________________________________________
  #
  # Multilevel Composite Reliability, multilevel.omega() -----------------------

  }, multilevel.omega = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Omega ####

    write.omega <- NULL
    if (isTRUE("omega" %in% write)) {

      #—————————————————————————————————————— #
      ### Extract Result Table ####

      write.omega <- write.object$omega

      #—————————————————————————————————————— #
      ### Round ####

      write.omega[, setdiff(colnames(write.omega), c("type", "items"))] <- sapply(write.omega[, setdiff(colnames(write.omega), c("type", "items"))], round, digits = digits)

      #—————————————————————————————————————— #
      ### Column Names ####

      if (isTRUE(x$args$se != "none")) {

        colnames(write.omega) <- c("Type", "Items", "Omega", "Low", "Upp")

      } else {

        colnames(write.omega) <- c("Type", "Items", "Omega")

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Item Statistics ####

    write.item <- NULL
    if (isTRUE("item" %in% write)) {

      #—————————————————————————————————————— #
      ### Extract Result Table ####

      write.item <- write.object$item

      #—————————————————————————————————————— #
      ### Round ####

      # Variables to round
      write.round <- switch(x$args$const,
                            within = c("pNA", "m", "sd", "min", "max", "skew", "kurt", "ICC", "wstd.ld"),
                            shared = c("pNA", "m", "sd", "min", "max", "skew", "kurt", "ICC", "bstd.ld"),
                            config = c("pNA", "m", "sd", "min", "max", "skew", "kurt", "ICC", "wstd.ld", "bstd.ld"))

      write.item[, write.round] <- sapply(write.item[, write.round], round, digits = digits)

      #—————————————————————————————————————— #
      ### Column Names ####

      colnames(write.item) <- switch(x$args$const,
                                     within = c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max",  "Skew", "Kurt", "ICC(1)", "WStd.ld"),
                                     shared = c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max",  "Skew", "Kurt", "ICC(1)", "BStd.ld"),
                                     config = c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max",  "Skew", "Kurt", "ICC(1)", "WStd.ld", "BStd.ld"))

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Write Object ####

    write.object <- list(Omega = write.omega, Itemstat = write.item)

  #_____________________________________________________________________________
  #
  # Auxiliary Variables Analysis, na.auxiliary() --------------------------------

  }, na.auxiliary = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Product-Moment Correlation matrix and Cohen's d Matrix ####

    if (isTRUE(is.null(x$args$model))) {

      # Round
      write.object$cor <- apply(write.object$cor, 2L, round, digits = digits)
      write.object$d <- apply(write.object$d, 2L, round, digits = digits)

      # Diagonals
      diag(write.object$cor) <- NA
      diag(write.object$d) <- NA

      # Lower and/or upper triangular
      switch(x$args$tri, "lower" = {

        write.object$cor[upper.tri(write.object$cor)] <- NA

      }, "upper" = {

        write.object$cor[lower.tri(write.object$cor)] <- NA

      })

      write.object$cor <- data.frame(rownames(write.object$cor), write.object$cor, fix.empty.names = FALSE)
      write.object$d <- data.frame(rownames(write.object$d), write.object$d, fix.empty.names = FALSE)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Semi-Partial Correlation Coefficients ####

    } else {

      # Standardized Solution
      write.object <- x$model.fit.stand

      # Outcome variable
      outcome <- setdiff(all.vars(as.formula(x$args$model)), attr(terms(as.formula(x$args$model)[-2L]), "term.labels"))

      # Select outcome rows
      write.object <- write.object[write.object$lhs == outcome, ]

      # Indices substantive model
      model.sub <- which(write.object$op == "~")

      # Indices auxiliary model
      model.aux <- which(write.object$op == "~~" & (write.object$lhs != write.object$rhs))

      # Round
      print.round <- c("est.std", "se", "z", "ci.lower", "ci.upper")

      write.object[, print.round] <- sapply(write.object[, print.round], round, digits)
      write.object$pvalue <- round(write.object$pval, digits = p.digits)

      # Names
      colnames(write.object) <- c("lhs", "op", "rhs", "Estimate", "Std.Err", "z-value", "pval", "Low", "Upp")

      # Models
      write.object <- data.frame(c("Substantive model", rep("", times = length(model.sub)),
                                           "Auxiliary model", rep("", times = length(model.aux))),
                                 rbind(rep(NA, times = 9L), write.object[model.sub, ], rep(NA, times = 9L), write.object[model.aux, ]), fix.empty.names = FALSE)

    }

  #_____________________________________________________________________________
  #
  # Variance-Covariance Coverage, na.coverage() --------------------------------

  }, na.coverage = {

    write.object <- sapply(data.frame(write.object), round, digits = digits)

    # Add variable names in the rows
    write.object <- data.frame(colnames(write.object), write.object,
                               row.names = NULL, check.rows = FALSE,
                               check.names = FALSE, fix.empty.names = FALSE)

  #_____________________________________________________________________________
  #
  # Descriptive Statistics for Missing Data, na.descript() ---------------------

  }, na.descript = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Level-1 Variables ####

    # At least one Level-1 variable
    if (isTRUE(any(!is.na(unlist(write.object$L1[-1L]))))) {

      # Round
      write.object$L1$no.missing.mean <- round(write.object$L1$no.missing.mean, digits = digits)
      write.object$L1$no.missing.sd <- round(write.object$L1$no.missing.sd, digits = digits)

      write.object$L1$perc.complete <- round(write.object$L1$perc.complete, digits = digits)
      write.object$L1$perc.incomplete <- round(write.object$L1$perc.incomplete, digits = digits)
      write.object$L1$perc.observed.values <- round(write.object$L1$perc.observed.values, digits = digits)
      write.object$L1$perc.missing.values <- round(write.object$L1$perc.missing.values, digits = digits)
      write.object$L1$perc.missing.mean <- round(write.object$L1$perc.missing.mean, digits = digits)
      write.object$L1$perc.missing.sd <- round(write.object$L1$perc.missing.sd, digits = digits)
      write.object$L1$perc.missing.min <- round(write.object$L1$perc.missing.min, digits = digits)
      write.object$L1$perc.missing.max <- round(write.object$L1$perc.missing.max, digits = digits)

      write.object$L1$table.miss.l1$pOb <- round(write.object$L1$table.miss.l1$pOb, digits = digits)
      write.object$L1$table.miss.l1$pNA <- round(write.object$L1$table.miss.l1$pNA, digits = digits)

      write.object.L1 <- data.frame(c("No. of cases", "No. of complete cases", "No. of incomplete cases", NA,
                                      "No. Of values", "No. Of observed values", "No of missing values", NA,
                                      "No. Of variables", "No. Of missing values across all variables",
                                      "   Mean", "   SD", "   Minimum", "   Maximum"),
                                    Freq = c(write.object$L1$no.cases, write.object$L1$no.complete, write.object$L1$no.incomplete, NA,
                                             write.object$L1$no.values, write.object$L1$no.observed.values, write.object$L1$no.missing.values, NA,
                                             write.object$L1$no.var, NA,
                                             write.object$L1$no.missing.mean, write.object$L1$no.missing.sd,
                                             write.object$L1$no.missing.min, write.object$L1$no.missing.max),
                                    Perc = c(NA, write.object$L1$perc.complete, write.object$L1$perc.incomplete, NA,
                                             NA, write.object$L1$perc.observed.values, write.object$L1$perc.missing.values, NA,
                                             NA, NA,
                                             write.object$L1$perc.missing.mean, write.object$L1$perc.missing.sd,
                                             write.object$L1$perc.missing.min, write.object$L1$perc.missing.max),
                                    row.names = NULL, check.rows = FALSE,
                                  check.names = FALSE, fix.empty.names = FALSE)

    # No Level-1 variable
    } else {

      write.object.L1 <- NULL

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Level-2 Variables ####

    # At least one Level-2 variable
    if (isTRUE(any(!is.na(unlist(write.object$L2[-1L]))))) {

      # Round
      write.object$L2$no.missing.mean <- round(write.object$L2$no.missing.mean, digits = digits)
      write.object$L2$no.missing.sd <- round(write.object$L2$no.missing.sd, digits = digits)

      write.object$L2$perc.complete <- round(write.object$L2$perc.complete, digits = digits)
      write.object$L2$perc.incomplete <- round(write.object$L2$perc.incomplete, digits = digits)
      write.object$L2$perc.observed.values <- round(write.object$L2$perc.observed.values, digits = digits)
      write.object$L2$perc.missing.values <- round(write.object$L2$perc.missing.values, digits = digits)
      write.object$L2$perc.missing.mean <- round(write.object$L2$perc.missing.mean, digits = digits)
      write.object$L2$perc.missing.sd <- round(write.object$L2$perc.missing.sd, digits = digits)
      write.object$L2$perc.missing.min <- round(write.object$L2$perc.missing.min, digits = digits)
      write.object$L2$perc.missing.max <- round(write.object$L2$perc.missing.max, digits = digits)

      write.object$L2$table.miss.l2$pOb <- round(write.object$L2$table.miss.l2$pOb, digits = digits)
      write.object$L2$table.miss.l2$pNA <- round(write.object$L2$table.miss.l2$pNA, digits = digits)

      write.object.L2 <- data.frame(c("No. of cases", "No. of complete cases", "No. of incomplete cases", NA,
                                      "No. Of values", "No. Of observed values", "No of missing values", NA,
                                      "No. Of variables", "No. Of missing values across all variables",
                                      "   Mean", "   SD", "   Minimum", "   Maximum"),
                                    Freq = c(write.object$L2$no.cluster.l2, write.object$L2$no.complete, write.object$L2$no.incomplete, NA,
                                             write.object$L2$no.values, write.object$L2$no.observed.values, write.object$L2$no.missing.values, NA,
                                             write.object$L2$no.var, NA,
                                             write.object$L2$no.missing.mean, write.object$L2$no.missing.sd,
                                             write.object$L2$no.missing.min, write.object$L2$no.missing.max),
                                    Perc = c(NA, write.object$L2$perc.complete, write.object$L2$perc.incomplete, NA,
                                             NA, write.object$L2$perc.observed.values, write.object$L2$perc.missing.values, NA,
                                             NA, NA,
                                             write.object$L2$perc.missing.mean, write.object$L2$perc.missing.sd,
                                             write.object$L2$perc.missing.min, write.object$L2$perc.missing.max),
                                    row.names = NULL, check.rows = FALSE,
                                    check.names = FALSE, fix.empty.names = FALSE)

    # No Level-2 variable
    } else {

      write.object.L2 <- NULL

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Level-3 Variables ####

    # At least one Level-3 variable
    if (isTRUE(any(!is.na(unlist(write.object$L3[-1L]))))) {

      # Round
      write.object$L3$no.missing.mean <- round(write.object$L3$no.missing.mean, digits = digits)
      write.object$L3$no.missing.sd <- round(write.object$L3$no.missing.sd, digits = digits)

      write.object$L3$perc.complete <- round(write.object$L3$perc.complete, digits = digits)
      write.object$L3$perc.incomplete <- round(write.object$L3$perc.incomplete, digits = digits)
      write.object$L3$perc.observed.values <- round(write.object$L3$perc.observed.values, digits = digits)
      write.object$L3$perc.missing.values <- round(write.object$L3$perc.missing.values, digits = digits)
      write.object$L3$perc.missing.mean <- round(write.object$L3$perc.missing.mean, digits = digits)
      write.object$L3$perc.missing.sd <- round(write.object$L3$perc.missing.sd, digits = digits)
      write.object$L3$perc.missing.min <- round(write.object$L3$perc.missing.min, digits = digits)
      write.object$L3$perc.missing.max <- round(write.object$L3$perc.missing.max, digits = digits)

      write.object$L3$table.miss.l3$pOb <- round(write.object$L3$table.miss.l3$pOb, digits = digits)
      write.object$L3$table.miss.l3$pNA <- round(write.object$L3$table.miss.l3$pNA, digits = digits)

      write.object.L3 <- data.frame(c("No. of cases", "No. of complete cases", "No. of incomplete cases", NA,
                                      "No. Of values", "No. Of observed values", "No of missing values", NA,
                                      "No. Of variables", "No. Of missing values across all variables",
                                      "   Mean", "   SD", "   Minimum", "   Maximum"),
                                    Freq = c(write.object$L3$no.cluster.l3, write.object$L3$no.complete, write.object$L3$no.incomplete, NA,
                                             write.object$L3$no.values, write.object$L3$no.observed.values, write.object$L3$no.missing.values, NA,
                                             write.object$L3$no.var, NA,
                                             write.object$L3$no.missing.mean, write.object$L3$no.missing.sd,
                                             write.object$L3$no.missing.min, write.object$L3$no.missing.max),
                                    Perc = c(NA, write.object$L3$perc.complete, write.object$L3$perc.incomplete, NA,
                                             NA, write.object$L3$perc.observed.values, write.object$L3$perc.missing.values, NA,
                                             NA, NA,
                                             write.object$L3$perc.missing.mean, write.object$L3$perc.missing.sd,
                                             write.object$L3$perc.missing.min, write.object$L3$perc.missing.max),
                                    row.names = NULL, check.rows = FALSE,
                                    check.names = FALSE, fix.empty.names = FALSE)

    # No Level-3 variable
    } else {

      write.object.L3 <- NULL

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Write Object ####

    write.object <- list(L1.Summary = write.object.L1, L2.Summary = write.object.L2, L3.Summary = write.object.L3,
                         L1.Table = write.object$L1$table.miss.l1, L2.Table = write.object$L2$table.miss.l2, L3.Table = write.object$L3$table.miss.l3)

    write.object <- write.object[sapply(write.object, function(y) !is.null(y))]

  #_____________________________________________________________________________
  #
  # Missing Data Pattern, na.pattern() -----------------------------------------
  }, na.pattern = {

    # Round
    write.object$perc <- round(write.object$perc, digits = digits)
    write.object$pNA <- round(write.object$pNA, digits = digits)

    names(write.object)[c(1L, 3L)] <- c("Pattern", "Perc")

  #_____________________________________________________________________________
  #
  # Model Comparison, modcomp() ------------------------------------------------
  }, modcomp = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Format ####

    # Round fit indices
    if (isTRUE(any(colnames(write.object) %in% c("deviance", "chisq", "d.chisq", "F", "cfi", "tli", "rmsea", "srmr", "srmrw", "srmrb")))) { intersect(colnames(write.object), c("deviance", "chisq", "d.chisq", "F", "cfi", "tli", "rmsea", "srmr", "srmrw", "srmrb")) |>
        (\(p) names(which(sapply(write.object[, p, drop = FALSE], function(y) !all(is.na(y))))))() |> (\(q) write.object[, q] <<- lapply(write.object[, q, drop = FALSE], function(y) sapply(y, function(z) round(z, digits = x$args$fit.digits))))() }

    # Round information criteria
    if (isTRUE(!is.null(x$args$print.ic))) { intersect(colnames(write.object), c("aic", "caic", "bic", "sabic", "aicc", "hqc", "hbic", "spbic", "ibic", "sic", "icomp")) |>
        (\(p) names(which(sapply(write.object[, p, drop = FALSE], function(y) !all(is.na(y))))))() |> (\(q) write.object[, q] <<- lapply(write.object[, q, drop = FALSE], function(y) sapply(y, function(z) round(z, digits = x$args$ic.digits))))() }

    # Round p-value
    if (isTRUE("p" %in% colnames(write.object))) { write.object$p <- round(write.object$p, digits = p.digits) }

    # Format
    colnames(write.object) <- misty::rec(colnames(write.object), spec = "'model' = 'Model'; 'npar' = '#Param'; 'deviance' = 'Deviance'; 'chisq' = 'Chisq'; 'cfi' = 'CFI'; 'tli' = 'TLI'; 'rmsea' = 'RMSEA'; 'srmr' = 'SRMR'; 'srmrw' = 'SRMRw'; 'srmrb' = 'SRMRb'; 'aic' = 'AIC'; 'caic' = 'CAIC'; 'bic' = 'BIC'; 'sabic' = 'SABIC'; 'aicc' = 'AICc'; 'hqc' = 'HQC'; 'hbic' = 'HBIC'; 'spbic' = 'SPBIC'; 'ibic' = 'IBIC'; 'sic' = 'SIC'; 'icomp' = 'ICOMP'; 'd.chisq' = 'dChisq'; 'd.df' = 'ddf'")

  #_____________________________________________________________________________
  #
  # Result Table for LCA Estimated in Mplus, mplus.lca.summa() -----------------
  }, mplus.lca.summa = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Extract Result Tables ####

    write.object.summary <- write.object$summary
    write.object.bf <- write.object$bf
    write.object.classif <- write.object$classif
    write.object.mean.var <- write.object$mean_var
    write.object.prob <- write.object$prob
    write.object.d <- write.object$d

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Summary Results ####

    #—————————————————————————————————————— #
    ### Round ####

    write.object.summary[, c("LL", "aic", "caic", "bic", "sabic", "awe", "occmin")] <- round(write.object.summary[, c("LL", "aic", "caic", "bic", "sabic", "awe", "occmin")], digits = digits)
    write.object.summary[, "LL.scale"] <- round(write.object.summary[, "LL.scale"], digits = digits + 2L)

    intersect(c("cmp", "chi.pear", "chi.lrt", "lmr.lrt", "almr.lrt", "blrt", "entropy", "avemin", "pmin"), colnames(write.object.summary)) |>
      (\(p) write.object.summary[, p] <<- apply(write.object.summary[, p, drop = FALSE], 2L, function(y) round(y, digits = p.digits)))()

    write.object.summary[, "nmin"] <- round(write.object.summary[, "nmin"], digits = 0L)

    #—————————————————————————————————————— #
    ### Column Names ####

    colnames(write.object.summary) <- misty::rec(colnames(write.object.summary), spec = "'folder' = 'Folder'; 'nclass' = '#Class'; 'conv' = 'Conv'; 'nparam' = '#Param'; 'LL' = 'logLik'; 'LL.scale' = 'Scale'; 'LL.rep' = 'LLRep'; 'aic' = 'AIC'; 'caic' = 'CAIC'; 'bic' = 'BIC'; 'sabic' = 'SABIC'; 'awe' = 'AWE'; 'cmp' = 'cmP'; 'lmr.lrt' = 'LMR-LRT'; 'almr.lrt' = 'A-LRT'; 'blrt' = 'BLRT'; 'chi.pear' = 'Chi-Pear'; 'chi.lrt' = 'Chi-LRT'; 'entropy' = 'Entropy'; 'avemin' = 'aPPMin'; 'occmin' = 'OCCMin'; 'nmin' = 'nMin'; 'pmin' = 'pMin'")

    #—————————————————————————————————————— #
    ### TRUE/FALSE into Yes/No ####

    write.object.summary$Conv <- sapply(write.object.summary$Conv, function(y) ifelse(isTRUE(y), "Yes", "No"))
    write.object.summary$LLRep <- sapply(write.object.summary$LLRep, function(y) ifelse(isTRUE(y), "Yes", "No"))

    #—————————————————————————————————————— #
    ### Additional Folder Row ####

    write.temp <- NULL
    for (i in unique(write.object.summary$Folder)) {

      write.temp <- rbind(write.temp, setNames(do.call(data.frame, list(i, rep(list(NA), times = ncol(write.object.summary) - 1L))), nm = colnames(write.object.summary)),
                                               write.object.summary[write.object.summary$Folder == i, ])

    }

    write.object.summary <- write.temp

    # Duplicated folder entries
    write.object.summary[duplicated(write.object.summary$Folder), "Folder"] <- NA

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Approximate Bayes Factor ####

    #—————————————————————————————————————— #
    ### Round ####

    write.object.bf[, c("A.bic", "B.bic", "bf")] <- round(write.object.bf[, c("A.bic", "B.bic", "bf")], digits = digits)

    #—————————————————————————————————————— #
    ### Truncate ####

    if (isTRUE(x$args$bf.trunc)) { write.object.bf$bf <- ifelse(write.object.bf$bf > 1000L, 1000L, write.object.bf$bf) }

    #—————————————————————————————————————— #
    ### Column Names ####

    colnames(write.object.bf) <- c("A-Folder", "A-#Class", "A-BIC", "B-Folder", "B-#Class", "B-BIC", "aBF")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Classification Diagnostics ####

    #—————————————————————————————————————— #
    ### Round ####

    intersect(c(colnames(write.object.classif)[substr(colnames(write.object.classif), 1L, 1L) == "p"], colnames(write.object.classif)[substr(colnames(write.object.classif), 1L, 3L) == "ave"]), colnames(write.object.classif)) |>
      (\(p) write.object.classif[, p] <<- round(write.object.classif[, p, drop = FALSE], digits = p.digits))()

    write.object.classif[, colnames(write.object.classif)[substr(colnames(write.object.classif), 1L, 3L) == "occ"]] <- round(write.object.classif[, colnames(write.object.classif)[substr(colnames(write.object.classif), 1L, 3L) == "occ"]], digits = digits)

    write.object.classif[substr(colnames(write.object.classif), 1L, 1L) == "n"] <- round(write.object.classif[substr(colnames(write.object.classif), 1L, 1L) == "n"], digits = 0L)

    #—————————————————————————————————————— #
    ### Column Names ####

    colnames(write.object.classif) <- misty::rec(colnames(write.object.classif), spec = "'folder' = 'Folder'; 'nclass' = '#Class'; 'conv' = 'Conv'; 'nparam' = '#Param'; 'LL.rep' = 'LLRep'; 'entropy' = 'Entropy'")

    colnames(write.object.classif) <- gsub("ave.pp", "aPP", colnames(write.object.classif))
    colnames(write.object.classif) <- gsub("occ", "OCC", colnames(write.object.classif))

    #—————————————————————————————————————— #
    ### TRUE/FALSE into Yes/No ####

    write.object.classif$Conv <- sapply(write.object.classif$Conv, function(y) ifelse(isTRUE(y), "Yes", "No"))
    write.object.classif$LLRep <- sapply(write.object.classif$LLRep, function(y) ifelse(isTRUE(y), "Yes", "No"))

    #—————————————————————————————————————— #
    ### Additional Folder Row ####

    write.temp <- NULL
    for (i in unique(write.object.classif$Folder)) {

      write.temp <- rbind(write.temp, setNames(do.call(data.frame, list(i, rep(list(NA), times = ncol(write.object.classif) - 1L))), nm = colnames(write.object.classif)),
                                               write.object.classif[write.object.classif$Folder == i, ])

    }

    write.object.classif <- write.temp

    # Duplicated folder entries
    write.object.classif[duplicated(write.object.classif$Folder), "Folder"] <- NA

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Means and Variances ####

    if (isTRUE(!is.null(write.object.mean.var))) {

      #### Round
      write.object.mean.var$n <- round(write.object.mean.var$n)

      write.object.mean.var$low <- round(write.object.mean.var$low, digits = p.digits)
      write.object.mean.var$upp <- round(write.object.mean.var$upp, digits = p.digits)

      #### Numeric
      write.object.mean.var$class <- as.numeric(write.object.mean.var$class)

      #### Column names
      colnames(write.object.mean.var) <- c("Folder", "#Class", "Class", "n", "Param", "Ind", "Est.", "SE", "z", "pval", "Low", "Upp")

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Probabilities ####

    if (isTRUE(!is.null(write.object.prob))) {

      #### Round
      write.object.prob$n <- round(write.object.prob$n)

      #### Numeric
      write.object.prob$class <- as.numeric(write.object.prob$class)
      write.object.prob$categ <- as.numeric(write.object.prob$categ)

      #### Column names
      colnames(write.object.prob) <- c("Folder", "#Class", "Class", "n", "Ind", "Categ", "Est.", "SE", "z", "pval")

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Cohen's d ####

    if (isTRUE(!is.null(write.object.d))) {

      #### Round
      write.object.d[, c("sd.j", "sd.k")] <- round(write.object.d[, c("sd.j", "sd.k")], digits = 3L)
      write.object.d[, "d"] <- round(write.object.d[, "d"], digits = p.digits)
      write.object.d[, c("n.j", "n.k")] <- round(write.object.d[, c("n.j", "n.k")], digits = 0L)

      #### Column names
      colnames(write.object.d) <- c("Folder", "#Class", "Ind", "Class.j", "Class.k", "n.j", "M.j", "SD.j", "n.k", "M.k", "SD.k", "d")

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Write Object ####

    # Combine result tables
    if (isTRUE(!is.null(write.object.mean.var))) {

      # Continuous indicators
      if (isTRUE(any(!x$result$mean_var$param == "Mean"))) {

        write.object <- Reduce(append, list(list(Summary = write.object.summary), list(aBF = write.object.bf), list(Classif = write.object.classif), list(Mean_Var = write.object.mean.var), list(d = write.object.d)))

      # Count indicators
      } else {

        write.object <- Reduce(append, list(list(Summary = write.object.summary), list(aBF = write.object.bf), list(Classif = write.object.classif), list(Mean = write.object.mean.var), list(d = write.object.d)))

      }

    # Categorical or Nominal indicators
    } else {

      write.object <- Reduce(append, list(list(Summary = write.object.summary), list(aBF = write.object.bf), list(Classif = write.object.classif), list(Prob = write.object.prob)))

    }

    # Remove NA list elements
    write.object <- write.object[sapply(write.object, function(y) any(!is.na(y)))]

  #_____________________________________________________________________________
  #
  # Robust Estimation of MLM and LMM, robust.lmer() ----------------------------
  }, robust.lmer = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Call ####

    write.object$call <- data.frame(c("Formula", "Data"), c(write.object$call$formula, write.object$call$data), fix.empty.names = FALSE)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Random Effects ####

    # Round variables
    write.object$randeff[, c("var", "sd")] <- sapply(c("var", "sd"), function(y) round(write.object$randeff[, y], digits = p.digits))
    write.object$randeff[, (grep("cor", colnames(write.object$randeff)):ncol(write.object$randeff))] <- round(write.object$randeff[, (grep("cor", colnames(write.object$randeff)):ncol(write.object$randeff))], digits = digits)

    # Replace NA with ""
    write.object$randeff[, c("groups", "name")] <- apply(write.object$randeff[, c("groups", "name")], 2L, function(y) gsub("NA", "  ", y))

    # Columns
    colnames(write.object$randeff) <- c("Groups", "Name", "Var", "SD", "Intercept", setdiff(colnames(write.object$randeff), c("groups", "name", "var", "sd", "cor")))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Coefficients ####

    if (isTRUE(!"p" %in% colnames(write.object$coef))) {

      # Round variables
      write.object$coef[, colnames(write.object$coef)] <- sapply(colnames(write.object$coef), function(y) round(write.object$coef[, y], digits = digits))

      # Columns
      write.object$coef <- data.frame(row.names(write.object$coef), write.object$coef, fix.empty.names = FALSE, row.names = NULL)

    } else {

      # Round variables
      write.object$coef[, setdiff(colnames(write.object$coef), "p")] <- sapply(setdiff(colnames(write.object$coef), "p"), function(y) round(write.object$coef[, y], digits = digits))
      write.object$coef[, "p"] <- round(write.object$coef[, "p"], digits = p.digits)

    }

    # Row names
    write.object$coef <- data.frame(row.names(write.object$coef), write.object$coef, fix.empty.names = FALSE, row.names = NULL)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Weights ####

    # Two-level model
    if (isTRUE(lme4::getME(x$model, name = "n_rtrms") == 1L)) {

      write.object$weight <- data.frame(Component = rep(c("Residual", "Random Effect"), each = 2L),
                                        Weight = rep(c("Weight = 1", "Weight != 1"), times = 2L),
                                        n = c(write.object$weight$resid$ew1, write.object$weight$resid$ew0, write.object$weight$ranef$bw1, write.object$weight$ranef$bw0))

    # Three-level model
    } else {

      write.object$weight <- data.frame(Component = rep(c("Residual", paste0("Random Effect ", names(lme4::getME(x$model, "w_b"))[1L]), paste0("Random Effect ", names(lme4::getME(x$model, "w_b"))[2L])), each = 2L),
                                        Weight = rep(c("Weight = 1", "Weight != 1"), times = 3L),
                                        n = c(write.object$weight$resid$ew1, write.object$weight$resid$ew0, write.object$weight$ranef1$b1w1, write.object$weight$ranef1$b1w0, write.object$weight$ranef2$b2w1, write.object$weight$ranef2$b2w0))

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Model Convergence ####

    if (isTRUE(!is.null(write.object$converg))) { write.object$converg <- data.frame(switch(as.character(write.object$converg), "1" = "Model converged", "0" = "Model singular", "-1" = "Model not converged"), fix.empty.names = FALSE) }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Write Object ####

    write.object <- list(Call = write.object$call, Randeff = write.object$randeff, Coef = write.object$coef, Weight = write.object$weight, Conv = write.object$converg)

  #_____________________________________________________________________________
  #
  # Print Summary Output -------------------------------------------------------
  }, summa = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Linear Regression, lm() ####

    if (isTRUE(all(class(x$model) == "lm"))) {

      #—————————————————————————————————————— #
      ### Call ####

      if (isTRUE("call" %in% write && !is.null(write.object$call))) { write.object$call <- data.frame(c("Formula", "Data"), c(write.object$call$formula, write.object$call$data), fix.empty.names = FALSE) }

      #—————————————————————————————————————— #
      ### Descriptive Statistics ####

      if (isTRUE(!is.null(write.object$descript))) {

        # Round variables
        write.object$descript[, c("m", "sd", "min", "p.min", "max", "p.max", "skew", "kurt")] <- round(write.object$descript[, c("m", "sd", "min", "p.min", "max", "p.max", "skew", "kurt")], digits = digits)

        # Column names
        colnames(write.object$descript) <- c("Variable", "n", "nUQ", "M", "SD", "Min", "%Min", "Max", "%Max", "Skew", "Kurt")

      }

      #—————————————————————————————————————— #
      ### Correlation Matrix ####

      if (isTRUE("cormat" %in% write && !is.null(write.object$cormat))) {

        # Round variables
        write.object$cormat <- sapply(data.frame(write.object$cormat), round, digits = digits)

        # Diagonal
        diag(write.object$cormat) <- NA

        # Lower triangular
        write.object$cormat[upper.tri(write.object$cormat)] <- NA

        # Row names
        write.object$cormat <- data.frame(colnames(write.object$cormat), write.object$cormat, fix.empty.names = FALSE)

      }

      #—————————————————————————————————————— #
      ### Model Summary ####

      if (isTRUE("modsum" %in% write && !is.null(write.object$modsum))) {

        # Round variables
        write.object$modsum[, c("R", "R2", "R2.adj", "p")] <- sapply(c("R", "R2", "R2.adj", "p"), function(y) round(write.object$modsum[, y], digits = p.digits))
        write.object$modsum[, "F"] <- round(write.object$modsum[, "F"], digits = digits)

      }

      #—————————————————————————————————————— #
      ### Coefficients ####

      if (isTRUE("coef" %in% write && !is.null(write.object$coef))) {

        # Round variables
        write.object$coef[, setdiff(colnames(write.object$coef), c("df", "p"))] <- sapply(setdiff(colnames(write.object$coef), c("df", "p")), function(y) round(write.object$coef[, y], digits = digits))
        write.object$coef[, "p"] <- round(write.object$coef[, "p"], digits = p.digits)

        # Row names
        write.object$coef <- data.frame(row.names(write.object$coef), write.object$coef, fix.empty.names = FALSE, row.names = NULL)

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Linear Mixed-Effects Model, lmer() ####

    } else if (isTRUE(all(class(x$model) %in% c("lmerMod", "lmerModLmerTest")))) {

      # Two-level model
      model.twolevel <- ifelse(lme4::getME(x$model, name = "n_rtrms") == 1L, TRUE, FALSE)

      #—————————————————————————————————————— #
      ### Call ####

      if (isTRUE("call" %in% write && !is.null(write.object$call))) { write.object$call <- data.frame(c("Formula", "Data"), c(write.object$call$formula, write.object$call$data), fix.empty.names = FALSE) }

      #—————————————————————————————————————— #
      ### Descriptive Statistics ####

      if (isTRUE("descript" %in% write && !is.null(write.object$descript))) {

        # Round variables
        write.object$descript[, c("m", "sd", "min", "p.min", "max", "p.max", "skew", "kurt")] <- round(write.object$descript[, c("m", "sd", "min", "p.min", "max", "p.max", "skew", "kurt")], digits = digits)

        # Two-Level Model
        if (isTRUE(model.twolevel)) {

          # Round ICC(1)
          write.object$descript[, "icc"] <- round(write.object$descript[, "icc"], digits = p.digits)

          # Row names
          colnames(write.object$descript) <- c("Variable", "n", "nUQ", "M", "SD", "Min", "%Min", "Max", "%Max", "Skew", "Kurt", "ICC(1)")

        # Three-Level Model
        } else {

          # Round ICC(1)
          write.object$descript[, c("icc.l2", "icc.l3")] <- sapply(c("icc.l2", "icc.l3"), function(y) round(write.object$descript[, y], digits = p.digits))

          # Row names
          colnames(write.object$descript) <- c("Variable", "n", "nUQ", "M", "SD", "Min", "%Min", "Max", "%Max", "Skew", "Kurt", "ICC(1)2", "ICC(1)3")

        }

      }

      #—————————————————————————————————————— #
      ### Correlation Matrix ####

      if (isTRUE("cormat" %in% write && !is.null(write.object$cormat))) {

        # Round and format
        write.object$cormat <- round(write.object$cormat, digits = digits)

        # Diagonal
        diag(write.object$cormat) <- NA

        # Row names
        write.object$cormat <- data.frame(colnames(write.object$cormat), write.object$cormat, fix.empty.names = FALSE)

      }

      #—————————————————————————————————————— #
      ### Model Summary ####

      if (isTRUE("modsum" %in% write && !is.null(write.object$modsum))) {

        # Round variables
        write.object$modsum[, c("margR2", "condR2")] <- sapply(c("margR2", "condR2"), function(y) round(write.object$modsum[, y], digits = p.digits))
        write.object$modsum[, c("loglik", "deviance")] <- sapply(c("loglik", "deviance"), function(y) round(write.object$modsum[, y], digits = digits))

        # Two-Level Model
        if (isTRUE(model.twolevel)) {

          # Row names
          if (isTRUE("nNA" %in% colnames(write.object$modsum))) {

            colnames(write.object$modsum) <-  c("n", "nNA", "nCL", "nPar", "Method", "logLik", "Deviance", "margR2", "condR2")

          } else {

            colnames(write.object$modsum) <-  c("n", "nCL", "nPar", "Method", "logLik", "Deviance", "margR2", "condR2")

          }

        # Three-Level Model
        } else {

          if (isTRUE("nNA" %in% colnames(write.object$modsum))) {

            colnames(write.object$modsum) <-  c("n", "nNA", "nCL2", "nCL3", "nPar", "Method", "logLik", "Deviance", "margR2", "condR2")

          } else {

            colnames(write.object$modsum) <-  c("n", "nCL2", "nCL3", "Method", "logLik", "Deviance", "margR2", "condR2")

          }

        }

      }

      #—————————————————————————————————————— #
      ### Random Effects ####

      if (isTRUE("randeff" %in% write && !is.null(write.object$randeff))) {

        # Round variables
        write.object$randeff[, c("var", "sd")] <- sapply(c("var", "sd"), function(y) round(write.object$randeff[, y], digits = p.digits))
        write.object$randeff[, (grep("cor", colnames(write.object$randeff)):ncol(write.object$randeff))] <- round(write.object$randeff[, (grep("cor", colnames(write.object$randeff)):ncol(write.object$randeff))], digits = digits)

        # Replace NA with ""
        write.object$randeff[, c("groups", "name")] <- apply(write.object$randeff[, c("groups", "name")], 2L, function(y) gsub("NA", "  ", y))

        # Columns
        colnames(write.object$randeff) <- c("Groups", "Name", "Var", "SD", "Intercept", setdiff(colnames(write.object$randeff), c("groups", "name", "var", "sd", "cor")))

      }

      #—————————————————————————————————————— #
      ### Coefficients ####

      if (isTRUE("coef" %in% write && !is.null(write.object$coef))) {

        if (isTRUE(all(class(x$model) == "lmerMod"))) {

          # Round variables
          write.object$coef[, setdiff(colnames(write.object$coef), "Level")] <- sapply(setdiff(colnames(write.object$coef), "Level"), function(y) round(write.object$coef[, y], digits = digits))

          # Columns
          write.object$coef <- data.frame(row.names(write.object$coef), write.object$coef, fix.empty.names = FALSE, row.names = NULL)

        } else if (isTRUE(all(class(x$model) == "lmerModLmerTest"))) {

          # Round variables
          write.object$coef[, setdiff(colnames(write.object$coef), c("p", "Level"))] <- sapply(setdiff(colnames(write.object$coef), c("p", "Level")), function(y) round(write.object$coef[, y], digits = digits))
          write.object$coef[, "p"] <- round(write.object$coef[, "p"], digits = p.digits)

        }

        # Row names
        write.object$coef <- data.frame(row.names(write.object$coef), write.object$coef, fix.empty.names = FALSE, row.names = NULL)

      }

      #—————————————————————————————————————— #
      ### Model Convergence ####

      if (isTRUE(!is.null(write.object$converg))) { write.object$converg <- data.frame(switch(as.character(write.object$converg), "1" = "Model converged", "0" = "Model singular", "-1" = "Model not converged"), fix.empty.names = FALSE) }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Linear Mixed-Effects Model, lme() ####

    } else if (isTRUE(all(class(x$model) %in% "lme"))) {

      # Two-level model
      model.twolevel <- ifelse(ncol(x$model$groups) == 1L, TRUE, FALSE)

      #—————————————————————————————————————— #
      ### Call ####

      if (isTRUE("call" %in% write && !is.null(write.object$call))) { write.object$call <- data.frame(c("Formula", "Data"), c(write.object$call$formula, write.object$call$data), fix.empty.names = FALSE) }

      #—————————————————————————————————————— #
      ### Descriptive Statistics ####

      if (isTRUE("descript" %in% write && !is.null(write.object$descript))) {

        # Round variables
        write.object$descript[, c("m", "sd", "min", "p.min", "max", "p.max", "skew", "kurt")] <- round(write.object$descript[, c("m", "sd", "min", "p.min", "max", "p.max", "skew", "kurt")], digits = digits)

        # Two-Level Model
        if (isTRUE(model.twolevel)) {

          # Round ICC(1)
          write.object$descript[, "icc"] <- round(write.object$descript[, "icc"], digits = p.digits)

          # Row names
          colnames(write.object$descript) <- c("Variable", "n", "nUQ", "M", "SD", "Min", "%Min", "Max", "%Max", "Skew", "Kurt", "ICC(1)")

        # Three-Level Model
        } else {

          # Round ICC(1)
          write.object$descript[, c("icc.l2", "icc.l3")] <- sapply(c("icc.l2", "icc.l3"), function(y) round(write.object$descript[, y], digits = p.digits))

          # Row names
          colnames(write.object$descript) <- c("Variable", "n", "nUQ", "M", "SD", "Min", "%Min", "Max", "%Max", "Skew", "Kurt", "ICC(1)2", "ICC(1)3")

        }

      }

      #—————————————————————————————————————— #
      ### Correlation Matrix ####

      if (isTRUE("cormat" %in% write && !is.null(write.object$cormat))) {

        # Round and format
        write.object$cormat <- round(write.object$cormat, digits = digits)

        # Diagonal
        diag(write.object$cormat) <- NA

        # Row names
        write.object$cormat <- data.frame(colnames(write.object$cormat), write.object$cormat, fix.empty.names = FALSE)

      }

      #—————————————————————————————————————— #
      ### Model Summary ####

      if (isTRUE("modsum" %in% write && !is.null(write.object$modsum))) {

        # Two-Level Model
        if (isTRUE(model.twolevel)) {

          # Round variables
          write.object$modsum[, c("margR2", "condR2")] <- sapply(c("margR2", "condR2"), function(y) round(write.object$modsum[, y], digits = p.digits))
          write.object$modsum[, c("loglik", "deviance")] <- sapply(c("loglik", "deviance"), function(y) round(write.object$modsum[, y], digits = digits))

          # Row names
          if (isTRUE("nNA" %in% colnames(write.object$modsum))) {

            colnames(write.object$modsum) <-  c("n", "nNA", "nCL", "nPar", "Method", "logLik", "Deviance", "margR2", "condR2")

          } else {

            colnames(write.object$modsum) <-  c("n", "nCL", "nPar", "Method", "logLik", "Deviance", "margR2", "condR2")

          }

        # Three-Level Model
        } else {

          # Round variables
          write.object$modsum[, c("loglik", "deviance")] <- sapply(c("loglik", "deviance"), function(y) round(write.object$modsum[, y], digits = digits))


          if (isTRUE("nNA" %in% colnames(write.object$modsum))) {

            colnames(write.object$modsum) <-  c("n", "nNA", "nCL2", "nCL3", "nPar", "Method", "logLik", "Deviance")

          } else {

            colnames(write.object$modsum) <-  c("n", "nCL2", "nCL3", "Method", "logLik", "Deviance")

          }

        }

      }

      #—————————————————————————————————————— #
      ### Random Effects ####

      if (isTRUE("randeff" %in% write && !is.null(write.object$randeff))) {

        #### Random Effects ####

        # Round variables
        write.object$randeff[, c("var", "sd")] <- sapply(c("var", "sd"), function(y) round(write.object$randeff[, y], digits = p.digits))

        # Replace NA with ""
        write.object$randeff[, c("groups", "name")] <- apply(write.object$randeff[, c("groups", "name")], 2L, function(y) gsub("NA", "  ", y))

        # Columns
        colnames(write.object$randeff) <- rec(colnames(write.object$randeff), spec = "'groups' = 'Groups'; 'name' = 'Name'; 'var' = 'Var'; 'sd' = 'SD'; 'cor' = 'Cor'", check = FALSE)

      }

      #—————————————————————————————————————— #
      ### Variance and Correlation Structure ####

      if (isTRUE("varcor" %in% write && !is.null(write.object$varcor))) {

        #...................
        #### Correlation Structure ####

        if (isTRUE(!is.null(write.object$varcor$corstruct))) {

          # Round
          write.object$varcor$corstruct$corstruct <- round(write.object$varcor$corstruct$corstruct, digits = digits)

          # Row names
          write.object$varcor$corstruct$corstruct <- setNames(data.frame(row.names(write.object$varcor$corstruct$corstruct), write.object$varcor$corstruct$corstruct, fix.empty.names = FALSE, row.names = NULL),
                                                              nm = c("", colnames(write.object$varcor$corstruct$corstruct)))

        }

        #...................
        #### Variance Function ####

        if (isTRUE(!is.null(write.object$varcor$varstruct))) {

          if (isTRUE(!is.null(write.object$varcor$varstruct$varstruct))) {

            if (isTRUE(!grepl("varComb", write.object$varcor$varstruct$class))) {

              # Round
              write.object$varcor$varstruct$varstruct <- round(write.object$varcor$varstruct$varstruct, digits = digits)

            # Combination of variance functions, varComb
            } else {

              # Round
              write.object$varcor$varstruct$varstruct <- lapply(write.object$varcor$varstruct$varstruct, round, digits = digits)

              # Combine
              write.object$varcor$varstruct$varstruct <- do.call("cbind", lapply(names(write.object$varcor$varstruct$varstruct), function(y) {

                setNames(data.frame(names(write.object$varcor$varstruct$varstruct[[y]]), t(write.object$varcor$varstruct$varstruct[[y]]), fix.empty.names = FALSE, row.names = NULL),
                         nm = c("", misty::chr.trim(sub("Variance function structure of class ", "", y))))

              }))

              # Names
              names(write.object$varcor$varstruct$varstruct)[which(!names(write.object$varcor$varstruct$varstruct) %in% c("varExp", "varPower", "varConstPower", "varConstProp", "varIdent", "varFixed", "varComb"))] <- ""

            }

          }

        }

      }

      #—————————————————————————————————————— #
      ### Coefficients ####

      if (isTRUE("coef" %in% write && !is.null(write.object$coef))) {

        # Round variables
        write.object$coef[, setdiff(colnames(write.object$coef), c("p", "Level"))] <- sapply(setdiff(colnames(write.object$coef), c("p", "Level")), function(y) round(write.object$coef[, y], digits = digits))
        write.object$coef[, "p"] <- round(write.object$coef[, "p"], digits = p.digits)

        # Row names
        write.object$coef <- data.frame(row.names(write.object$coef), write.object$coef, fix.empty.names = FALSE, row.names = NULL)

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Write Object ####

    write.object <- list(Call = write.object$call, Descript = write.object$descript, Cormat = write.object$cormat, Modsum = write.object$modsum, Randeff = write.object$randeff, CorStruct = write.object$varcor$corstruct$corstruct, VarStruct = write.object$varcor$varstruct$varstruct, Coef = write.object$coef, Conv = write.object$converg)

  #_____________________________________________________________________________
  #
  # Levene's Test for Homogeneity of Variance ----------------------------------
  }, test.levene = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Descriptive Statistics ####

    if (isTRUE(!is.null(write.object$descript))) {

      # Round variables
      write.object$descript[, c("m", "sd", "var", "low", "upp", "skew", "kurt")] <- round(write.object$descript[, c("m", "sd", "var", "low", "upp", "skew", "kurt")], digits = digits)

      # Column names
      colnames(write.object$descript) <- c("Group", "n", "nNA", "M", "SD", "Var", "Low", "Upp", "Skew", "Kurt")

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Levene's Test ####

    # Round variables
    write.object$test[, c("SS", "MSS", "F")] <- round(write.object$test[, c("SS", "MSS", "F")], digits = digits)
    write.object$test[, "p"] <- round(write.object$test[, "p"], digits = p.digits)

    # Column names
    write.object$test <- data.frame(row.names(write.object$test), write.object$test, fix.empty.names = FALSE)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Write Object ####

    write.object <- list(Descript = write.object$descript, Levene = write.object$test)

  #_____________________________________________________________________________
  #
  # Welch's Test ---------------------------------------------------------------
  }, test.welch = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Welch t-Test ####

    switch(x$sample, two = {

      # Round
      write.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "t", "df", "d", "d.low", "d.upp")] <- round(write.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "t", "df", "d", "d.low", "d.upp")], digits = digits)
      write.object[, "pval"] <- round(write.object[, "pval"], digits = p.digits)

      # Column names
      colnames(write.object) <- c("Group", "n", "nNA", "M", "SD", "M.Diff", "SE", "Low", "Upp", "t", "df", "p", "d", "Low", "Upp")

      # Remove Cohen's d
      if (isTRUE(!x$args$effsize)) { write.object <- write.object[, -c(13:15)] }

      # Remove descriptive statistics
      if (isTRUE(!x$args$descript)) { write.object <- na.omit(write.object[, -c(1L:9L)]) }

      # Write object
      write.object <- list(Welch = write.object)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Welch ANOVA ####

    }, multiple = {

      #—————————————————————————————————————— #
      ### Round ####

      write.object[["descript"]][, c("m", "low", "upp", "sd", "skew", "kurt")] <- round(write.object[["descript"]][, c("m", "low", "upp", "sd", "skew", "kurt")], digits = digits)

      write.object[["test"]][, c("F", "df2", "eta.sq", "omega.sq")] <- round(write.object[["test"]][, c(c("F", "df2", "eta.sq", "omega.sq"))], digits = digits)
      write.object[["test"]][, "pval"] <- round(write.object[["test"]][, "pval"], digits = p.digits)

      write.object[["posthoc"]][, c("m.diff", "se", "m.low", "m.upp", "t", "df", "d", "d.low", "d.upp")] <- round(write.object[["posthoc"]][, c("m.diff", "se", "m.low", "m.upp", "t", "df", "d", "d.low", "d.upp")], digits = digits)
      write.object[["posthoc"]][, "pval"] <- round(write.object[["posthoc"]][, "pval"], digits = p.digits)

      #...................
      ### Column Names  ####

      colnames(write.object[["descript"]]) <- c("Group", "n", "nNA", "M", "Low", "Upp", "SD", "Skew", "Kurt")
      colnames(write.object[["test"]]) <- c("F", "df1", "df2", "p", "eta.sq", "omega.sq")
      colnames(write.object[["posthoc"]]) <- c("Group1", "Group2", "M.diff", "SE", "Low", "Upp", "t", "df", "p", "d", "Low", "Upp")

      #### Remove Results ####

      # Descriptive statistics
      if (isTRUE(!x$args$descript)) { write.object[["descript"]] <- NULL }

      # Effect sizes
      if (isTRUE(!x$args$effsize)) {

        write.object[["test"]] <- write.object[["test"]][, -which(colnames(write.object[["test"]]) %in% c("eta.sq", "omega.sq"))]

        write.object[["posthoc"]] <- write.object[["posthoc"]][, -c(7:9)]

      }

      # Post-hoc test
      if (isTRUE(!x$args$posthoc)) { write.object[["posthoc"]] <- NULL }

      #...................
      ### Write Object  ####

      write.object <- list(ANOVA = write.object$test, Descript = write.object$descript, PostHoc = write.object$posthoc)

    })

  #_____________________________________________________________________________
  #
  # z-Test ---------------------------------------------------------------------
  }, test.z = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## One sample z-Test ####

    switch(x$sample, "one" = {

      #—————————————————————————————————————— #
      ### Round ####

      write.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "z", "d")] <- round(write.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "z", "d")], digits = digits)
      write.object[, "p"] <- formatC(write.object[, "p"], digits = p.digits)

      #—————————————————————————————————————— #
      ### Column Names ####

      colnames(write.object) <- c("n", "nNA", "M", "SD", "M.Diff", "SE", "Low", "Upp", "z", "p", "d")

      #—————————————————————————————————————— #
      ### Remove Columns ####

      # Cohen's d
      if (isTRUE(!x$args$effsize)) { write.object <- write.object[, -which(colnames(write.object) %in% c("d"))] }

      # Descriptive statistics
      if (isTRUE(!x$args$descript)) { write.object <- na.omit(write.object[, -which(colnames(write.object) %in% c("n", "Group", "nNA", "M", "SD", "M.Diff", "SE", "Low", "Upp"))]) }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Two-Sample z-Test ####

    }, "two" = {

      #—————————————————————————————————————— #
      ### Round ####

      write.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "z", "d")] <- round(write.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "z", "d")], digits = digits)
      write.object[, "p"] <- round(write.object[, "p"], digits = p.digits)

      #—————————————————————————————————————— #
      ### Column Names ####

      colnames(write.object) <- c("Group", "n", "nNA", "M", "SD", "M.Diff", "SE", "Low", "Upp", "z", "p", "d")

      #—————————————————————————————————————— #
      ### Remove Columns ####

      # Cohen's d
      if (isTRUE(!x$args$effsize)) { write.object <- write.object[, -which(colnames(write.object) %in% c("d"))] }

      # Descriptive statistics
      if (isTRUE(!x$args$descript)) { write.object <- write.object[, -which(colnames(write.object) %in% c("n", "nNA", "M", "SD", "M.Diff", "SE", "Low", "Upp"))] }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Paired-Sample z-Test ####

    }, "paired" = {

      #—————————————————————————————————————— #
      ### Round ####

      write.object[, c("m1", "m2", "m.diff", "sd.diff", "se", "m.low", "m.upp", "z", "d")] <- round(write.object[, c("m1", "m2", "m.diff", "sd.diff", "se", "m.low", "m.upp", "z", "d")], digits = digits)
      write.object[, "p"] <- round(write.object[, "p"], digits = p.digits)

      #—————————————————————————————————————— #
      ### Column Names ####

      colnames(write.object) <- c("n", "nNA", "M1", "M2", "M.Diff", "SD.Diff", "SE", "Low", "Upp", "z", "p", "d")

      #—————————————————————————————————————— #
      ### Remove Columns ####

      # Cohen's d
      if (isTRUE(!x$args$effsize)) { write.object <- write.object[, -which(colnames(write.object) %in% c("d"))] }

      # Descriptive statistics
      if (isTRUE(!x$args$descript)) { write.object <- write.object[, -which(colnames(write.object) %in% c("n", "nNA", "M1", "M2", "M.Diff", "SD.Diff", "SE", "Low", "Upp"))] }

    })

  #_____________________________________________________________________________
  #
  # t-Test ---------------------------------------------------------------------
  }, test.t = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## One sample t-Test ####

    switch(x$sample, "one" = {

      #—————————————————————————————————————— #
      ### Round ####

      write.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "z", "d")] <- round(write.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "z", "d")], digits = digits)
      write.object[, "p"] <- formatC(write.object[, "p"], digits = p.digits)

      #—————————————————————————————————————— #
      ### Column Names ####

      colnames(write.object) <- c("n", "nNA", "M", "SD", "M.Diff", "SE", "Low", "Upp", "z", "p", "d")

      #—————————————————————————————————————— #
      ### Remove Columns ####

      # Cohen's d
      if (isTRUE(!x$args$effsize)) { write.object <- write.object[, -which(colnames(write.object) %in% c("d"))] }

      # Descriptive statistics
      if (isTRUE(!x$args$descript)) { write.object <- na.omit(write.object[, -which(colnames(write.object) %in% c("n", "Group", "nNA", "M", "SD", "M.Diff", "SE", "Low", "Upp"))]) }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Two-Sample t-Test ####

    }, "two" = {

      #—————————————————————————————————————— #
      ### Round ####

      write.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "z", "d")] <- round(write.object[, c("m", "sd", "m.diff", "se", "m.low", "m.upp", "z", "d")], digits = digits)
      write.object[, "p"] <- round(write.object[, "p"], digits = p.digits)

      #—————————————————————————————————————— #
      ### Column Names ####

      colnames(write.object) <- c("Group", "n", "nNA", "M", "SD", "M.Diff", "SE", "Low", "Upp", "z", "p", "d")

      #—————————————————————————————————————— #
      ### Remove Columns ####

      # Cohen's d
      if (isTRUE(!x$args$effsize)) { write.object <- write.object[, -which(colnames(write.object) %in% c("d"))] }

      # Descriptive statistics
      if (isTRUE(!x$args$descript)) { write.object <- write.object[, -which(colnames(write.object) %in% c("n", "nNA", "M", "SD", "M.Diff", "SE", "Low", "Upp"))] }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Paired-Sample t-Test ####

    }, "paired" = {

      #—————————————————————————————————————— #
      ### Round ####

      write.object[, c("m1", "m2", "m.diff", "sd.diff", "se", "m.low", "m.upp", "z", "d")] <- round(write.object[, c("m1", "m2", "m.diff", "sd.diff", "se", "m.low", "m.upp", "z", "d")], digits = digits)
      write.object[, "p"] <- round(write.object[, "p"], digits = p.digits)

      #—————————————————————————————————————— #
      ### Column Names ####

      colnames(write.object) <- c("n", "nNA", "M1", "M2", "M.Diff", "SD.Diff", "SE", "Low", "Upp", "z", "p", "d")

      #—————————————————————————————————————— #
      ### Remove Columns ####

      # Cohen's d
      if (isTRUE(!x$args$effsize)) { write.object <- write.object[, -which(colnames(write.object) %in% c("d"))] }

      # Descriptive statistics
      if (isTRUE(!x$args$descript)) { write.object <- write.object[, -which(colnames(write.object) %in% c("n", "nNA", "M1", "M2", "M.Diff", "SD.Diff", "SE", "Low", "Upp"))] }

    })

  #_____________________________________________________________________________
  #
  # Extract Unique Elements and Count Number of Unique Elements ----------------
  }, uniq = {

    # Convert into data frame
    write.object <- list(Uniq = as.data.frame(lapply(write.object, function(z) c(z, rep(NA, times = max(sapply(write.object, length)) - length(z))))))

  })

  #_____________________________________________________________________________
  #
  # Write Excel file -----------------------------------------------------------

  # Exclude NULL elements
  write.object <- write.object[!sapply(write.object, is.null)]

  # Write results
  misty::write.xlsx(write.object, file = file)

  # Return write object
  return(invisible(write.object))

}

#_______________________________________________________________________________
