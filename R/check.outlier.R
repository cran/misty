#' Outlier and Influential Observations Diagnostics for Linear Models
#'
#' This function performs outlier and influential observations diagnostics by plotting
#' for statistical measures for leverage, distance, and influence for linear models
#' estimated by using the \code{lm()} function. \emph{Mahalanobis distance} and
#' \emph{hat values} are computed for quantifying \strong{leverage},
#' \emph{standardized leverage-corrected residuals} and \emph{studentized leverage-corrected residuals}
#' are computed for quantifying \strong{distance}, and \emph{Cookꞌs distance} and
#' standardized \emph{DFBETAS} are computed for quantifying \strong{influence}.
#' By default, the function provides plots showing observations with the 10\%
#' highest absolute values or a minimum of 10 observations with the highest absolute
#' values in the \emph{hat values}, \emph{studentized leverage-corrected residuals},
#' and \emph{Cookꞌs distances}.
#'
#' @param model    a fitted model of class \code{"lm"}.
#' @param type     a character vector indicating which statistical measures to
#'                 include in the plot, i.e., \code{"mahal"} for the Mahalanobis
#'                 distance, \code{"hat"} for the hat value, \code{"rstand"}
#'                 for the standardized leverage-corrected residual, \code{"rstud"}
#'                 for the studentized leverage-corrected residual, \code{"cook"}
#'                 for the Cook's distance, and \code{"dfb"} for the DFBETAS.
#' @param cutoff   a named numeric vector indicating the cutoff values for the
#'                 statistical measures shown in the plot as horizontal lines.
#'                 Note that the elements of the vectors need to be need with
#'                 \code{"mahal"}, \code{"hat"}, \code{"rstand"}, \code{"rstud"},
#'                 \code{"cook"}, and \code{"dfb"}. For example,
#'                 \code{cutoff = c(hat = 0.2, cook = 0.3)} to draw
#'                 a horizontal line at \code{0.2} for the hat value plot and a
#'                 horizontal line at \code{0.3} for the Cook's distance plot.
#' @param sample   a numeric value for selecting the sample with the highest
#'                 absolute values in the statistical measures for leverage, distance,
#'                 and influence. The value is either a proportion between 0 and 1,
#'                 e.g., \code{sample = 0.2} for the 20\% highest absolute values,
#'                 or an integer value for selecting the number of observations
#'                 with the highest absolute values. Note that the logical argument
#'                 \code{FALSE} can be used to plot all observations. By default,
#'                 the function plots observations with the 10\% highest absolute
#'                 values, but a minimum of 10 observations.
#' @param sort     logical: if \code{TRUE} (default), observations are sorted
#'                 in decreasing order by the statistical measure.
#' @param append   logical: if \code{TRUE} (default), statistical measures for
#'                 leverage, distance, and influence are appended to the data frame
#'                 in \code{model$model}.
#' @param plot     logical: if \code{TRUE} (default), a plot is drawn.
#' @param geom     a character vector indicating which plot elements to draw, i.e.,
#'                 \code{"none"} for not drawing any labels, \code{"text"} (default)
#'                 for drawing labels above or below each observation.
#' @param filename a character string indicating the \code{filename}
#'                 argument including the file extension in the \code{ggsave}
#'                 function. Note that one of \code{".eps"}, \code{".ps"},
#'                 \code{".tex"}, \code{".pdf"} (default),
#'                 \code{".jpeg"}, \code{".tiff"}, \code{".png"},
#'                 \code{".bmp"}, \code{".svg"} or \code{".wmf"} needs
#'                 to be specified as file extension in the \code{file}
#'                 argument. Note that plots can only be saved when
#'                 \code{plot = TRUE}.
#' @param width    a numeric value indicating the \code{width} argument
#'                 (default is the size of the current graphics device)
#'                 for the \code{ggsave} function.
#' @param height   a numeric value indicating the \code{height} argument
#'                 (default is the size of the current graphics device)
#'                 for the \code{ggsave} function.
#' @param dpi      a numeric value indicating the \code{dpi} argument
#'                 (default is \code{600}) for the \code{ggsave} function.
#' @param check  logical: if \code{TRUE} (default), argument specification is checked.
#' @param ...    further arguments to be passed to or from methods.
#'
#' @details
#' In regression analysis, an observation can be extreme in three major ways (see
#' Darlington & Hayes, 2017, p. 484): (1) An observation has high \strong{leverage} if
#' it has a atypical pattern of values on the predictors, (2) an observation has
#' high \strong{distance} if its observed outcome value \eqn{Y_i} has a large
#' deviation from the predicted value \eqn{\hat{Y}_i}, and (3) an observation has
#' high \strong{influence} if its inclusion substantially changes the estimates
#' for the intercept and/or slopes.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{check.resid}}, \code{\link{check.collin}}
#'
#' @references
#' Darlington, R. B., &, Hayes, A. F. (2017). \emph{Regression analysis and linear
#' models}: Concepts, applications, and implementation. The Guilford Press.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{model}}{model specified in \code{model }}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{plot}}{ggplot2 object for plotting the residuals}
#' \item{\code{data}}{list with data frame, i.e., \code{data} for a data frame
#'                    in \code{model$model} with statistical measures for leverage,
#'                    distance, and influence are appended to the data frame when
#'                    \code{append = TRUE}, \code{mahal} for the sample of \code{data}
#'                    with the highest Mahalanobis distance, \code{hat} for the
#'                    sample of \code{data} with the highest hat values, \code{rstand}
#'                    for the sample of \code{data} with the highest absolute
#'                    standardized leverage-corrected residuals, \code{rstud} for
#'                    the sample  of \code{data} with the highest absolute studentized
#'                    leverage-corrected residuals, \code{cook} for the sample of
#'                    \code{data} with  the highest Cookꞌs distance, and \code{*.dfb}
#'                    for the sample of \code{data} with the highest absolute DFBETAS}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #————————————————————————————————————————————————————————————————————————————
#' # Outlier and Influential Observations Diagnostics for Linear Models
#'
#' # Estimate linear model
#' mod <- lm(mpg ~ cyl + disp + hp, data = mtcars)
#'
#' #——————————————————————————————————————
#' ## Default Setting
#'
#' # Example 1a: Hat values, studentized leverage-corrected residuals, and Cook's distance
#' check.outlier(mod)
#'
#' # Example 1b: Additional plot arguments
#' # see Details in the help page of the function plot.misty.object
#' check.outlier(mod, point.shape = 1, point.size = 4, line.col = "red")
#'
#' #——————————————————————————————————————
#' ## Argument 'cutoff' and 'type'
#'
#' # Example 2a: Horizontal cutoff lines
#' check.outlier(mod, cutoff = c(hat = 0.3, rstud = 2, cook = 0.1))
#'
#' # Example 2b: Mahalanobis distance and standardized leverage-corrected residuals
#' check.outlier(mod, type = c("mahal", "rstand"), cutoff = c(mahal = 8, rstand = 2))
#'
#' # Example 2c: DFBETAS
#' check.outlier(mod, type = "dfb", cutoff = c(dfb = 0.35))
#'
#' #——————————————————————————————————————
#' ## Arguments 'sample' and 'sort'
#'
#' # Example 3a: Plot entire data set and do not sort
#' check.outlier(mod, sample = FALSE, sort = FALSE)
#'
#' # Example 3b: Plot 50% highest absolute values
#' check.outlier(mod, sample = 0.5)
#'
#' # Example 3c: Plot n = 15 highest absolute values
#' check.outlier(mod, sample = 15)
#'
#' #——————————————————————————————————————
#' ## Data
#'
#' object <- check.outlier(mod)
#'
#' # Example 4a: Data frame with with statistical measures
#' object$data$data
#'
#' # Example 4b: Sample of data with the highest hat values
#' object$data$hat
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Save Plot
#'
#' # Example 7: Save plot as PNG fine
#' check.outlier(mod, filename = "Outlier.png", width = 12, height = 15)
#' }
check.outlier <- function(model, type = c("mahal", "hat", "rstand", "rstud", "cook", "dfb"),
                          cutoff = NULL, sample = 0.10, sort = TRUE, append = TRUE,
                          plot = TRUE, geom = c("none", "text"), filename = NULL,
                          width = NA, height = NA, dpi = 600, check = TRUE, ...) {

  #_____________________________________________________________________________
  #
  # Initial Check ——————————————————————————————————————————————————————————————

  # Check if input 'model' is missing or NULL
  if (isTRUE(missing(model) || is.null(model))) { stop("Please specify a fitted model for the argument 'model'.", call. = FALSE) }

  # Check if input 'model' is not 'lm'
  if (isTRUE(!inherits(model, "lm"))) { stop("Please specify an \"lm\" object for the argument 'model'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ————————————————————————————————————————————————————————————————

  .check.input(logical = c("sort", "append", "plot"), s.character = list(geom = c("none", "text")), m.character = list(type = c("mahal", "hat", "rstand", "rstud", "cook", "dfb")),
               envir = environment(), input.check = check)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Additional Checks ####

  if (isTRUE(check)) {

    # 'sample' argument
    if (isTRUE(sample < 0L || sample > nrow(model$model))) { stop(paste0("Please specify a number greater 0 and smaller than ", nrow(model$model), " for the argument 'sample'."), call. = FALSE) }

    # 'cutoff' argument
    if (isTRUE(!is.null(cutoff))) {

      if (isTRUE(is.null(names(cutoff)))) { stop("Please specify a named vector for the argument 'cutoff'.", call. = FALSE) }

      if (isTRUE(any(!names(cutoff) %in% c("mahal", "hat", "rstand", "rstud", "cook", "dfb")))) { stop("Please use the element names \"mahal\", \"hat\", \"rstand\", \"rstud\", \"cook\", or \"dfb\" for the argument 'cutoff'.", call. = FALSE) }

    }

  }

  #_____________________________________________________________________________
  #
  # Data and Variables —————————————————————————————————————————————————————————

  # Data
  mod.dat <- model$model

  # Intercept and predictors
  mod.int.pred <- names(model$coefficients)

  # Predictors
  mod.pred <- misty::chr.omit(mod.int.pred, "(Intercept)")

  #_____________________________________________________________________________
  #
  # Arguments ——————————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'sample' Argument ####

  # All observations
  if (isTRUE(!sample)) {

    n.sample <- nrow(model$model)

  # Sample
  } else {

    # Proportion
    if (isTRUE(sample %% 1L != 0L)) {

      # By default, 10% sample and at least 10 observations
      n.sample <- ceiling(nrow(mod.dat) * sample) |> (\(p) if (isTRUE(p < 10L)) { ifelse(nrow(mod.dat) < 10L, nrow(mod.dat), 10L) } else { return(p) })()

    # Number of observations
    } else {

      n.sample <- sample

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'type' Argument ####

  if (isTRUE(all(c("mahal", "hat", "rstand", "rstud", "cook", "dfb") %in% type))) { type <- c("hat", "rstud", "cook") }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'geom' Argument ####

  if (isTRUE(all(c("none", "text") %in% geom))) { geom <- "text" }

  #_____________________________________________________________________________
  #
  # Main Function ——————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Leverage ####

  # Mahalanobis distance
  mod.mahal <- mahalanobis(mod.dat[, mod.pred], center = colMeans(mod.dat[, mod.pred], na.rm = TRUE), cov = cov(mod.dat[, mod.pred], use = "pairwise.complete.obs"))

  # Hat values
  mod.hat <- hatvalues(model)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Distance ####

  # Standardized leverage-corrected residual
  mod.rstand <- rstandard(model)

  # Studentized leverage-corrected residual
  mod.rstud <- rstudent(model)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Influence ####

  # Cook's distance
  mod.cook <- cooks.distance(model)

  # Standardized DFBETAS
  mod.dfbeta <- setNames(as.data.frame(dfbetas(model)), nm = paste0(misty::chr.gsub(c("\\(", "\\)"), c("", ""), mod.int.pred), ".dfb"))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Result table ####

  result <- merge(data.frame(idout = if (isTRUE(all(!is.na(suppressWarnings(as.numeric(row.names(mod.dat))))))) { as.numeric(row.names(mod.dat)) } else { row.names(mod.dat) }, mahal = mod.mahal),
                  data.frame(idout = if (isTRUE(all(!is.na(suppressWarnings(as.numeric(row.names(mod.dfbeta))))))) { as.numeric(row.names(mod.dfbeta)) } else { row.names(mod.dfbeta) },
                             hat = mod.hat, rstand = mod.rstand, rstud = mod.rstud, cook = mod.cook, mod.dfbeta),
                  by = "idout", sort = FALSE, all.x = TRUE)

  # Missing values
  if (isTRUE(length(model[[9L]]) != 0L)) {

    na <- model[[9L]]
    insert <- sapply(colnames(result), function(z) z = NA)

    idout <- result$idout
    inerst.row <- names(na)

    for (i in seq_along(na)) {

      na.i <- seq(from = na[i], to = nrow(result))
      result[na.i + 1L, ] <- result[na.i, ]
      result[na[i], ] <- insert

      idout[na.i + 1] <- idout[na.i]
      idout[na[i]] <- inerst.row[i]

    }

    result$idout <- idout

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Append ####

  if (isTRUE(append)) {

    # Missing values
    if (isTRUE(length(model[[9L]]) != 0L)) {

      insert <- sapply(colnames(model$model), function(z) z = NA)

      for (i in seq_along(na)) {

        na.i <- seq(from = na[i], to = nrow(mod.dat))
        mod.dat[na.i + 1L, ] <- mod.dat[na.i, ]
        mod.dat[na[i], ] <- insert

      }

    }

    object <- data.frame(idout = result$idout, mod.dat, result[, -grep("idout", colnames(result))], row.names = NULL)

  } else {

    object <- result

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Sample ####

  #—————————————————————————————————————— #
  ### Leverage ####

  # Mahalanobis distance
  object.mahal <- object[which(object$mahal >= sort(object$mahal, decreasing = TRUE)[n.sample]), ] |> (\(p) if (isTRUE(sort)) { p[order(p$mahal, decreasing = TRUE), ] } else { return(p) })()

  # Hat values
  object.hat <- object[which(object$hat >= sort(object$hat, decreasing = TRUE)[n.sample]), ] |> (\(p) if (isTRUE(sort)) { p[order(p$hat, decreasing = TRUE), ] } else { return(p) })()

  #—————————————————————————————————————— #
  ### Distance ####

  # Standardized leverage-corrected residual
  object.rstand <- object[which(abs(object$rstand) >= sort(abs(object$rstand), decreasing = TRUE)[n.sample]), ] |> (\(p) if (isTRUE(sort)) { p[order(abs(p$rstand), decreasing = TRUE), ] } else { return(p) })()

  # Studentized leverage-corrected residual
  object.rstud <- object[which(abs(object$rstud) >= sort(abs(object$rstud), decreasing = TRUE)[n.sample]), ] |> (\(p) if (isTRUE(sort)) { p[order(abs(p$rstud), decreasing = TRUE), ] } else { return(p) })()

  #—————————————————————————————————————— #
  ### Influence ####

  # Cook's distance
  object.cook <- object[which(object$cook >= sort(object$cook, decreasing = TRUE)[n.sample]), ] |> (\(p) if (isTRUE(sort)) { p[order(p$cook, decreasing = TRUE), ] } else { return(p) })()

  # Standardized DFBETAS
  for (i in paste0(misty::chr.gsub(c("\\(", "\\)"), c("", ""), mod.int.pred), ".dfb")) { assign(paste0("object.", i), object[which(abs(object[, i]) >= sort(abs(object[, i]), decreasing = TRUE)[n.sample]), ] |> (\(p) if (isTRUE(sort)) { p[order(abs(p[, i]), decreasing = TRUE), ] } else { return(p) })()) }

  #_____________________________________________________________________________
  #
  # Return object ——————————————————————————————————————————————————————————————

  object <- list(call = match.call(),
                 type = "check.outlier",
                 model = model,
                 args = list(type = type, cutoff = cutoff, sample = sample, sort = sort, append = append, plot = plot, geom = geom, filename = filename, width = width, height = height, dpi = dpi),
                 plot = NULL,
                 data = setNames(eval(parse(text = paste0("Reduce(append, list(list(object), list(object.mahal), list(object.hat), list(object.rstand), list(object.rstud), list(object.cook), ", paste0("list(object.", misty::chr.gsub(c("\\(", "\\)"), c("", ""), mod.int.pred), ".dfb)", collapse = ", "), "))"))),
                                 nm = c("data", "mahal", "hat", "rstand", "rstud", "cook", paste0(misty::chr.gsub(c("\\(", "\\)"), c("", ""), mod.int.pred), ".dfb"))))

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  ## Plot and Save Plot ————————————————————————————————————————————————————————

  if (isTRUE(plot)) { object$plot <- plot(object, filename = filename, width = width, height = height, dpi = dpi, check = FALSE, ...) |> (\(p) suppressMessages(suppressWarnings(print(p))))() }

  #_____________________________________________________________________________
  #
  # Output —————————————————————————————————————————————————————————————————————

  return(invisible(object))

}

#_______________________________________________________________________________
