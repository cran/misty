#' Blimp Trace Plots and Posterior Distribution Plots
#'
#' This function reads the posterior distribution including burn-in and
#' post-burn-in phase for all parameters saved in long format in a file called
#' \code{posterior.*} by the function \code{blimp.run} or \code{blimp} when
#' specifying \code{posterior = TRUE} to display trace plots and posterior
#' distribution plots.
#'
#' @param x                 a character string indicating the name of folder
#'                          containing the \code{posterior.*} file, e.g.,
#'                          \code{"Posterior_Ex4.3"} or the name of the
#'                          \code{posterior.*} file with or without any file
#'                          extension, e.g., \code{"Posterior_ExEx4.3/posterior.csv"}
#'                          or \code{"Posterior_ExEx4.3/posterior"}. Alternatively,
#'                          a \code{misty.object} of type \code{blimp} can be
#'                          specified, i.e., result object of the \code{blimp.plot()}
#'                          function. Note that if the \code{posterior} file is
#'                          specified without file extension while multiple
#'                          \code{posterior.*} files in different file formats
#'                          are available, then the file is read in following
#'                          order: \code{csv},\code{RData}, \code{rds},
#'                          and \code{xlsx}.
#' @param plot              a character string indicating the type of plot to
#'                          display, i.e., \code{"none"} for not displaying any
#'                          plot, \code{"trace"} (default) for displaying trace
#'                          plots, and \code{post} for displaying posterior
#'                          distribution plots.
#' @param param             a numeric vector indicating which parameters to print
#'                          for the trace plots or posterior distribution plots.
#'                          Note that the number of the parameter (\code{Param})
#'                          and the parameter specification (\code{L1}, \code{L2},
#'                          and \code{L3}) are provided in the text file
#'                          \code{"partable.txt"}. Note that parameters with zero
#'                          variance are excluded by default.
#' @param burnin            logical: if \code{FALSE}, the burn-in iterations are
#'                          discarded when displaying trace plots. The default
#'                          setting for \code{plot = "trace"} is \code{TRUE}.
#'                          Note that the burn-in iterations are always discarded
#'                          when displaying posterior distribution plots
#'                          (\code{plot = "post"}) regardless of the setting of
#'                          the argument \code{burnin}.
#' @param point             a character vector indicating the point estimate(s)
#'                          to be displayed in the posterior distribution plots,
#'                          i.e., \code{"all"} for all point estimates, \code{"none"}
#'                          for not displaying any point estimates, \code{"m"}
#'                          for the posterior mean estimate, \code{"med"} (default)
#'                          for the posterior median estimate, and \code{"map"}
#'                          for the maximum a posteriori estimate.
#' @param ci                a character string indicating the type of credible
#'                          interval to be displayed in the posterior distribution
#'                          plots, i.e., \code{"none"} for not displaying any
#'                          credible intervals, \code{"eti"} (default) for displaying
#'                          the equal-tailed intervals and \code{"hdi"} for displaying
#'                          the highest density interval.
#' @param conf.level        a numeric value between 0 and 1 indicating the
#'                          confidence level of the credible interval (default is
#'                          \code{0.95}).
#' @param hist              logical: if \code{TRUE} (default), histograms are
#'                          drawn in the posterior probability plots.
#' @param density           logical: if \code{TRUE} (default), density curves are
#'                          drawn in the posterior probability plots.
#' @param area              logical: if \code{TRUE} (default), statistical not
#'                          significant and statistical significant area is
#'                          filled with a different color and vertical lines are
#'                          drawn.
#' @param alpha             a numeric value between 0 and 1 for the \code{alpha}
#'                          argument (default is \code{0.4}) for the \code{annotate},
#'                          and \code{geom_histogram} function.
#' @param fill              a character string indicating the color for the
#'                          \code{"fill"} argument (default is \code{"gray85"})
#'                          for the \code{annotate} and \code{geom_histogram}
#'                          functions.
#' @param nrow              a numeric value indicating the \code{nrow} argument
#'                          (default is \code{NULL}) for the \code{facet_wrap}
#'                          function.
#' @param ncol              a numeric value indicating the \code{ncol} argument
#'                          (default is \code{2}) for the \code{facet_wrap} function.
#' @param scales            a character string indicating the \code{scales} argument
#'                          (default is \code{"free"}) for the \code{facet_wrap}
#'                          function.
#' @param xlab              a character string indicating the \code{name} argument
#'                          for the \code{scale_x_continuous} function.
#' @param ylab              a character string indicating the \code{name} argument
#'                          for the \code{scale_y_continuous} function.
#' @param xlim              a numeric vector with two elements indicating the
#'                          \code{limits} argument (default it \code{NULL}) for
#'                          the \code{scale_x_continuous} function.
#' @param ylim              a numeric vector with two elements indicating the
#'                          \code{limits} argument (default it \code{NULL}) for
#'                          the \code{scale_y_continuous} function.
#' @param xbreaks           a numeric vector indicating the \code{breaks} argument
#'                          (default is \code{ggplot2::waiver()}) for the
#'                          \code{scale_x_continuous} function.
#' @param ybreaks           a numeric vector indicating the \code{breaks} argument
#'                          (default is \code{ggplot2::waiver()}) for the
#'                          \code{scale_y_continuous} function.
#' @param xexpand           a numeric vector with two elements indicating the
#'                          \code{expand} argument (default is \code{(0.02, 0)})
#'                          for the \code{scale_x_continuous} function.
#' @param yexpand           a numeric vector with two elements indicating the
#'                          \code{expand} argument for the \code{scale_y_continuous}
#'                          function. Note that the default setting depends
#'                          on the type of plot, e.g., \code{(0.02, 0)} for the
#'                          trace plots and \code{expansion(mult = c(0, 0.05))}
#'                          for the posterior distribution plots.
#' @param palette           a character string indicating the palette name (default
#'                          is \code{"Set 2"}) for the \code{hcl.colors} function.
#'                          Note that the character string must be one of
#'                          \code{hcl.pals()}.
#' @param binwidth          a numeric value indicating the \code{binwidth} argument
#'                          (default is to use the number of bins in \code{bins}
#'                          argument) for the \code{geom_histogram} function.
#' @param bins              a numeric value indicating the \code{bins} argument
#'                          (default is \code{30}) for the \code{geom_histogram}
#'                          function.
#' @param density.col       a character string indicating the \code{color} argument
#'                          (default is \code{"#0072B2"}) for the \code{geom_density}
#'                          function.
#' @param shape             a numeric value indicating the \code{shape} argument
#'                          (default is \code{21}) for the \code{geom_point}
#'                          function.
#' @param point.col         a character vector with three elements indicating the
#'                          \code{values} argument (default is
#'                          \code{c("#CC79A7", "#D55E00", "#009E73")}) for the
#'                          \code{scale_color_manual} function.
#' @param linewidth         a numeric value indicating the \code{linewidth} argument
#'                          (default is \code{0.6}) for the \code{geom_vline} function.
#' @param linetype          a numeric value indicating the \code{linetype} argument
#'                          (default is \code{"dashed"}) for the \code{geom_vline}
#'                          function.
#' @param line.col          a character string indicating the \code{color} argument
#'                          (default is \code{"black"}) for the \code{geom_vline}
#'                          function.
#' @param plot.margin       a numeric vector indicating the \code{plot.margin}
#'                          argument for the \code{theme} function. Note that the
#'                          default setting depends on the type of the plot, e.g.,
#'                          \code{c(4, 15, -10, 0)} for the trace plots, and
#'                          \code{c(4, 15, 4, 4)} for the autocorrelation plots.
#' @param legend.title.size a numeric value indicating the \code{legend.title}
#'                          argument (default is \code{element_text(size = 10)})
#'                          for the \code{theme} function.
#' @param legend.text.size  a numeric value indicating the \code{legend.text}
#'                          argument (default is \code{element_text(size = 10)})
#'                          for the \code{theme} function.
#' @param legend.box.margin a numeric vector indicating the \code{legend.box.margin}
#'                          argument for the \code{theme} function. Note that the
#'                          default setting depends on the type of plot, e.g.,
#'                          \code{c(-16, 6, 6, 6)} for the trace plots, and
#'                          \code{c(-25, 6, 6, 6)} for the posterior distribution
#'                          plots with displaying point estimates.
#' @param saveplot          a character vector indicating the plot to be saved,
#'                          i.e., \code{"all"} for saving all plots, \code{"none"}
#'                          (default) for not saving any plots, \code{"trace"}
#'                          for saving the trace plots and \code{post} for the saving
#'                          the posterior distribution plots.
#' @param file              a character string indicating the \code{filename}
#'                          argument (default is \code{"Blimp_Plot.pdf"}) including
#'                          the file extension for the \code{ggsave} function.
#'                          Note that one of \code{".eps"}, \code{".ps"},
#'                          \code{".tex"}, \code{".pdf"} (default), \code{".jpeg"},
#'                          \code{".tiff"}, \code{".png"}, \code{".bmp"},
#'                          \code{".svg"} or \code{".wmf"} needs to be specified
#'                          as file extension in the \code{file} argument.
#' @param file.plot         a character vector with two elements for distinguishing
#'                          different types of plots. By default, the character
#'                          string specified in the argument \code{"file"}
#'                          (\code{"Blimp_Plot"}) is concatenated with \code{"_TRACE"}
#'                          (\code{"Blimp_Plot_TRACE"}) for the trace plots,
#'                          and \code{"_POST"} (\code{"Blimp_Plot_POST"}) for
#'                          the posterior distribution plots.
#' @param width             a numeric value indicating the \code{width} argument
#'                          (default is the size of the current graphics device)
#'                          for the \code{ggsave} function.
#' @param height            a numeric value indicating the \code{height} argument
#'                          (default is the size of the current graphics device)
#'                          for the \code{ggsave} function.
#' @param units             a character string indicating the \code{units} argument
#'                          (default is \code{in}) for the \code{ggsave} function.
#' @param dpi               a numeric value indicating the \code{dpi} argument
#'                          (default is \code{600}) for the \code{ggsave} function.
#' @param check             logical: if \code{TRUE} (default), argument
#'                          specification is checked.
#'
#' @author
#' Takuya Yanagida
#'
#' @seealso
#' \code{\link{blimp}}, \code{\link{blimp.update}}, \code{\link{blimp.run}},
#' \code{\link{blimp.print}}, \code{\link{blimp.plot}}, \code{\link{blimp.bayes}}
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
#' \item{\code{x}}{a character string indicating the name of the \code{posterior.*}}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{data}}{list with posterior distribution of each parameter estimate
#'                    in long format (\code{plotdat}), plot data for the trace
#'                    plots (\code{trace}), and plot data for the posterior
#'                    distribution plots (\code{post}).}
#' \item{\code{plot}}{list with the trace plots (\code{trace} and posterior distribution
#'                    plots (\code{post})}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #----------------------------------------------------------------------------
#' # Blimp Example 4.3: Linear Regression
#'
#' #..........
#' # Trace Plots
#'
#' # Example 1a: Default setting, specifying name of the folder
#' blimp.plot("Posterior_Ex4.3")
#'
#' # Example 1b: Default setting, specifying the posterior file
#' blimp.plot("Posterior_Ex4.3/posterior.csv")
#'
#' # Example 1c: Print parameters 2, 3, 4, and 5
#' blimp.plot("Posterior_Ex4.3", param = 2:5)
#'
#' # Example 1e: Arrange panels in three columns
#' blimp.plot("Posterior_Ex4.3", ncol = 3)
#'
#' # Example 1f: Specify "Pastel 1" palette for the hcl.colors function
#' blimp.plot("Posterior_Ex4.3", palette = "Pastel 1")
#'
#' #..........
#' # Posterior Distribution Plots
#'
#' # Example 2a: Default setting, i.e., posterior median and equal-tailed interval
#' blimp.plot("Posterior_Ex4.3", plot = "post")
#'
#' # Example 2b: Display posterior mean and maximum a posteriori
#' blimp.plot("Posterior_Ex4.3", plot = "post", point = c("m", "map"))
#'
#' # Example 2c: Display maximum a posteriori and highest density interval
#' blimp.plot("Posterior_Ex4.3", plot = "post", point = "map", ci = "hdi")
#'
#' # Example 2d: Do not display any point estimates and credible interval
#' blimp.plot("Posterior_Ex4.3", plot = "post", point = "none", ci = "none")
#'
#' # Example 2d: Do not display histograms
#' blimp.plot("Posterior_Ex4.3", plot = "post", hist = FALSE)
#'
#' #..........
#' # Save Plots
#'
#' # Example 3a: Save all plots in pdf format
#' blimp.plot("Posterior_Ex4.3", saveplot = "all")
#'
#' # Example 3b: Save all plots in png format with 300 dpi
#' blimp.plot("Posterior_Ex4.3", saveplot = "all", file = "Blimp_Plot.png", dpi = 300)
#'
#' # Example 3a: Save posterior distribution plot, specify width and height of the plot
#' blimp.plot("Posterior_Ex4.3", plot = "none", saveplot = "poast",
#'            width = 7.5, height = 7)
#'
#' #----------------------------------------------------------------------------
#' # Plot from misty.object
#'
#' # Create misty.object
#' object <- blimp.plot("Posterior_Ex4.3", plot = "none")
#'
#' # Trace plot
#' blimp.plot(object, plot = "trace")
#'
#' # Posterior distribution plot
#' blimp.plot(object, plot = "post")
#'
#' #----------------------------------------------------------------------------
#' # Create Plots Manually
#'
#' # Load ggplot2 package
#' library(ggplot2)
#'
#' # Create misty object
#' object <- blimp.plot("Posterior_Ex4.3", plot = "none")
#'
#' #..........
#' # Example 4: Trace Plots
#'
#' # Extract data
#' data.trace <- object$data$trace
#'
#' # Plot
#' ggplot(data.trace, aes(x = iter, y = value, color = chain)) +
#'   annotate("rect", xmin = 0, xmax = 1000, ymin = -Inf, ymax = Inf,
#'            alpha = 0.4, fill = "gray85") +
#'   geom_line() +
#'   facet_wrap(~ param, ncol = 2, scales = "free") +
#'   scale_x_continuous(name = "", expand = c(0.02, 0)) +
#'   scale_y_continuous(name = "", expand = c(0.02, 0)) +
#'   scale_colour_manual(name = "Chain",
#'                       values = hcl.colors(n = 2, palette = "Set 2")) +
#'   theme_bw() +
#'   guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
#'   theme(plot.margin = margin(c(4, 15, -10, 0)),
#'         legend.position = "bottom",
#'         legend.title = element_text(size = 10),
#'         legend.text = element_text(size = 10),
#'         legend.box.margin = margin(c(-16, 6, 6, 6)),
#'         legend.background = element_rect(fill = "transparent"))
#'
#' #..........
#' # Example 5: Posterior Distribution Plots
#'
#' # Extract data
#' data.post <- object$data$post
#'
#' # Plot
#' ggplot(data.post, aes(x = value)) +
#'   geom_histogram(aes(y = after_stat(density)), color = "black", alpha = 0.4,
#'                  fill = "gray85") +
#'   geom_density(color = "#0072B2") +
#'   geom_vline(data = data.frame(param = levels(data.post$param),
#'                                stat = tapply(data.post$value, data.post$param, median)),
#'              aes(xintercept = stat, color = "Median"), linewidth = 0.6) +
#'   geom_vline(data = data.frame(param = levels(data.post$param),
#'                                low = tapply(data.post$value, data.post$param,
#'                                             function(y) quantile(y, probs = 0.025))),
#'              aes(xintercept = low), linetype = "dashed", linewidth = 0.6) +
#'   geom_vline(data = data.frame(param = levels(data.post$param),
#'                                upp = tapply(data.post$value, data.post$param,
#'                                             function(y) quantile(y, probs = 0.975))),
#'              aes(xintercept = upp), linetype = "dashed", linewidth = 0.6) +
#'   facet_wrap(~ param, ncol = 2, scales = "free") +
#'   scale_x_continuous(name = "", expand = c(0.02, 0)) +
#'   scale_y_continuous(name = "Probability Density, f(x)",
#'                      expand = expansion(mult = c(0L, 0.05))) +
#'   scale_color_manual(name = "Point Estimate", values = c(Median = "#D55E00")) +
#'   labs(caption = "95% Equal-Tailed Interval") +
#'   theme_bw() +
#'   theme(plot.margin = margin(c(4, 15, -8, 4)),
#'         plot.caption = element_text(hjust = 0.5, vjust = 7),
#'         legend.position = "bottom",
#'         legend.title = element_text(size = 10),
#'         legend.text = element_text(size = 10),
#'         legend.box.margin = margin(c(-30, 6, 6, 6)),
#'         legend.background = element_rect(fill = "transparent"))
#' }
blimp.plot <- function(x, plot = c("none", "trace", "post"), param = NULL,
                       burnin = TRUE, point = c("all", "none", "m", "med", "map"),
                       ci = c("none", "eti", "hdi"), conf.level = 0.95, hist = TRUE,
                       density = TRUE, area = TRUE, alpha = 0.4, fill = "gray85",
                       nrow = NULL, ncol = NULL, scales = c("fixed", "free", "free_x", "free_y"),
                       xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL,
                       xbreaks = ggplot2::waiver(), ybreaks = ggplot2::waiver(),
                       xexpand = ggplot2::waiver(), yexpand = ggplot2::waiver(),
                       palette = "Set 2", binwidth = NULL, bins = NULL,
                       density.col = "#0072B2", shape = 21,
                       point.col = c("#CC79A7", "#D55E00", "#009E73"),
                       linewidth = 0.6, linetype = "dashed",
                       line.col = "black",
                       plot.margin = NULL, legend.title.size = 10, legend.text.size = 10,
                       legend.box.margin = NULL, saveplot = c("all", "none", "trace", "post"),
                       file = "Blimp_Plot.pdf", file.plot = c("_TRACE", "_POST"),
                       width = NA, height = NA, units = c("in", "cm", "mm", "px"),
                       dpi = 600, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check input 'x'
  if (isTRUE(missing(x))) { stop("Please specify a character string indicating the name of folder or name of the posterior data file for the argument 'x'", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Character string ####

  if (isTRUE(is.character(x))) {

    # Character string
    if (isTRUE(length(x) != 1L)) { stop("Please specify a character string indicating the name of the folder or name of the posterior data file for the argument 'x'", call. = FALSE) }

    # Folder
    if (isTRUE(dir.exists(x))) {

      x <- list.files(x, pattern = "posterior.", full.names = TRUE) |>
             (\(z) if (isTRUE(length(z) == 0L)) { stop("There is no \"posterior\" file in the folder specified in 'x'.", call. = FALSE) } else { return(z[1L]) })()

    # Data file
    } else {

      # With file extension
      if (isTRUE(any(sapply(c(".csv", ".xlsx", ".rds", ".RData"), grepl, x)))) {

        if (isTRUE(!file.exists(x))) { stop(paste0("Unable to read the \"posterior\" file: ", sQuote(x), " does not exist."), call. = FALSE) }

      # No file extension provided
      } else {

        x <- which(sapply(c(".csv", ".xlsx", ".rds", ".RData"), function(y) file.exists(paste0(x, y)))) |>
               (\(z) if(length(z) == 0L) { stop(paste0("Unable to read the \"posterior\" file: ", sQuote(x), " does not exist."), call. = FALSE) } else { return(names(z)) } )() |>
               (\(w) paste0(x, w[1L]))()

      }

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## misty object ####

  } else if (isTRUE(class(x) == "misty.object")) {

    if (isTRUE(x$type != "blimp")) { stop("Please specify a \"blimp\" object for the argument 'x'.", call. = FALSE) }

  } else {

    stop("Please specify a \"misty.object\" or a character string indicating the name of the folder or name of the posterior data file for the argument 'x'", call. = FALSE)

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # R package ggplot2
    if (isTRUE(!nzchar(system.file(package = "ggplot2")))) { stop("Package \"ggplot2\" is needed for this function, please install the package.", call. = FALSE) }

    # Check input 'plot'
    if (isTRUE(!all(plot %in% c("none", "trace", "post")))) { stop("Character string in the argument 'plot' does not match with \"none\", \"trace\", \"post\", \"auto\", \"ppc\", or \"loop\".", call. = FALSE) }

    if (isTRUE(!all(c("none", "trace", "post") %in% plot) && length(plot) != 1L)) { stop("Please specify \"none\", \"trace\", \"post\", \"auto\", \"ppc\", or \"loop\" for the argument 'plot'.", call. = FALSE) }

    # Check input 'burnin'
    if (isTRUE(!is.logical(burnin))) { stop("Please specify TRUE or FALSE for the argument 'burnin'.", call. = FALSE) }

    # Check input 'point'
    if (isTRUE(!all(point %in% c("all", "none", "m", "med", "map")))) { stop("Character strings in the argument 'point' do not all match with \"all\", \"none\", \"m\", \"med\", or \"map\".", call. = FALSE) }

    # Check input 'ci'
    if (isTRUE(!all(ci %in% c("none", "eti", "hdi")))) { stop("Character string in the argument 'ci' does not match with \"none\", \"eti\", or \"hdi\".", call. = FALSE) }

    if (isTRUE(!all(c("none", "eti", "hdi") %in% ci) && length(ci) != 1L)) { stop("Please specify \"none\", \"eti\", or \"hdi\" for the argument 'ci'.", call. = FALSE) }

    # Check input 'conf.level'
    if (isTRUE(conf.level >= 1L || conf.level <= 0L)) { stop("Please specifiy a numeric value between 0 and 1 for the argument 'conf.level'.", call. = FALSE) }

    # Check input 'hist'
    if (isTRUE(!is.logical(hist))) { stop("Please specify TRUE or FALSE for the argument 'hist'.", call. = FALSE) }

    # Check input 'density'
    if (isTRUE(!is.logical(density))) { stop("Please specify TRUE or FALSE for the argument 'density'.", call. = FALSE) }

    # Check input 'area'
    if (isTRUE(!is.logical(area))) { stop("Please specify TRUE or FALSE for the argument 'area'.", call. = FALSE) }

    # Check input 'alpha '
    if (isTRUE(alpha  >= 1L || alpha  <= 0L)) { stop("Please specifiy a numeric value between 0 and 1 for the argument 'alpha '.", call. = FALSE) }

    # Check input 'nrow'
    if (isTRUE(!is.null(nrow) && (nrow %% 1L != 0L || nrow < 0L))) { stop("Please specify  a positive integer number for the argument 'nrow'.", call. = FALSE) }

    # Check input 'ncol'
    if (isTRUE(!is.null(nrow) && (ncol %% 1L != 0L || ncol < 0L))) { stop("Please specify a positive integer number for the argument 'ncol'.", call. = FALSE) }

    # Check input 'scales'
    if (isTRUE(!all(scales %in% c("fixed", "free", "free_x", "free_y")))) { stop("Character strings in the argument 'scales' do not all match with \"fixed\", \"free\", \"free_x\", or \"free_y\".", call. = FALSE) }

    # Check input 'palette'
    if (isTRUE(!all(palette %in% hcl.pals()))) { stop("Character string in the argument 'palette' does not match with color palettes in hcl.pals().", call. = FALSE) }

    # Check input 'point.col'
    if (isTRUE(length(point.col) != 3L)) { stop("Please specify character vector with three elements for the argument 'point.col'.", call. = FALSE) }

    # Check input 'plot.margin'
    if (isTRUE(!is.null(plot.margin) && length(plot.margin) != 4L)) { stop("Please specify a numeric vector with four elements for the argument 'plot.margin'.", call. = FALSE) }

    # Check input 'legend.box.margin'
    if (isTRUE(!is.null(legend.box.margin) && length(legend.box.margin) != 4L)) { stop("Please specify numeric vector with four elements for the argument 'legend.box.margin'.", call. = FALSE) }

    # Check input 'saveplot'
    if (isTRUE(!all(saveplot %in% c("all", "none", "trace", "post")))) { stop("Character strings in the argument 'saveplot' do not all match with \"all\", \"none\", \"trace\", \"post\", \"ppc\", or \"loop\".", call. = FALSE) }

    # Check input 'file.plot'
    if (isTRUE(length(file.plot) != 2L)) { stop("Please specify a character vector with two elements for the argument 'file.plot'.", call. = FALSE) }

    # Check input 'units'
    if (isTRUE(!all(units %in% c("in", "cm", "mm", "px")))) { stop("Character string in the argument 'units' does not match with \"in\", \"cm\", \"mm\", \"pdf\", or \"px\".", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'plot' Argument ####

  # Default setting
  if (isTRUE(all(c("none", "trace", "post") %in% plot))) { plot <- "trace" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'param' Argument ####

  # Default setting
  if (isTRUE(all(c("all", "on", "by", "with", "inter", "var", "r2", "new") %in% param))) {

    param <- "on"

    # All input commands
  } else if (isTRUE("all" %in% param)) {

    param <- c("on", "by", "with", "inter", "var", "r2", "new")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'point' Argument ####

  # Default setting
  if (isTRUE(all(c("all", "none", "m", "med", "map") %in% point))) {

    point <- "med"

  # All input commands
  } else if (isTRUE("all" %in% point)) {

    point <- c("m", "med", "map")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'ci' Argument ####

  # Default setting
  if (isTRUE(all(c("none", "eti", "hdi") %in% ci))) { ci <- "eti" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'scales' Argument ####

  # Default setting
  if (isTRUE(all(c("free", "free_x", "free_y") %in% scales))) { scales <- "free" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'nrow' and 'ncol' Argument ####

  if (isTRUE(is.null(nrow) && is.null(ncol))) { ncol <- 2L }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'saveplot' Argument ####

  # Default setting
  if (isTRUE(all(c("all", "none", "trace", "post") %in% saveplot))) {

    saveplot <- "none"

  } else if (isTRUE("all" %in% saveplot)) {

    saveplot <- c("trace", "post")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'units' Argument ####

  # Default setting
  if (isTRUE(all(c("in", "cm", "mm", "px") %in% units))) { units <- "in" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  posterior <- iter <- value <- chain <- stat <- low <- upp <- NULL

  #----------------------------------------
  # Blimp Plot Data in CSV File
  if (isTRUE(class(x) != "misty.object")) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Read Blimp Posterior File ####

    tryCatch(

      switch(names(which(sapply(c(".csv", ".xlsx", ".rds", ".RData"), grepl, x))),
             # CSV format
             ".csv" =   {

               if (isTRUE(ncol(read.csv(x, nrows = 1L)) > 1L)) {

                 plotdat <- read.csv(x)

               } else {

                 plotdat <- utils::read.csv2(x)

               }

               },
             # Excel format
             ".xlsx" =  { plotdat <- misty::read.xlsx(x) },
             # RDS format
             ".rds" =   { plotdat <- readRDS(x) },
             # R workspace
             ".RData" = {

               load(x)

               plotdat <- posterior

               rm(posterior)

               }),

      error = function(y) { stop("Reading posterior file specified in the argument 'x' failed.", call. = FALSE) })

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Data Preparation ####

    #...................
    ### Number of Iterations and Number of Chains ####

    # Number of iterations
    n.burnin <- max(plotdat[which(plotdat$postburn == 0), "iter"])
    n.postburn <- max(plotdat[which(plotdat$postburn == 1), "iter"])

    # Number of chains
    n.chains <- max(plotdat$chain)

    #...................
    ### Exclude Parameters with Zero Variance ####

    plotdat <- which(tapply(plotdat$value, plotdat$param, var) == 0L) |>
      (\(z) if (isTRUE(length(z) != 0L)) {

       return(plotdat[which(!plotdat$param %in% z), ])

      } else {

        return(plotdat)

      })()

    #...................
    ### Select Parameters ####

    if (isTRUE(!is.null(param))) {

      plotdat.post <- plotdat.trace <- plotdat[which(plotdat$param %in% param), ]

    } else {

      plotdat.post <- plotdat.trace <- plotdat

    }

    #...................
    ### Factors ####

    plotdat.post$postburn <- plotdat.trace$postburn <- factor(plotdat.trace$postburn, levels = unique(plotdat.trace$postburn))
    plotdat.post$chain <- plotdat.trace$chain <- factor(plotdat.trace$chain)

    #...................
    ### Label ####

    plotdat.post$param <- plotdat.trace$param <- factor(paste0("Parameter ", plotdat.trace$param), levels = paste0("Parameter ", sort(unique(plotdat.trace$param))))

    #...................
    ### Discard burn-in iterations ####

    if (isTRUE(!burnin)) { plotdat.trace <- plotdat.trace[which(!plotdat.trace$postburn == 0L), ] }

    plotdat.post <- plotdat.post[which(!plotdat.post$postburn == 0L), ]

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Trace Plots ####

    # 'xlab' argument
    if (isTRUE(is.null(xlab))) { xlab.trace <- "Iteration" } else { xlab.trace <- xlab }

    # 'ylab' argument
    if (isTRUE(is.null(ylab))) { ylab.trace <- "" } else { ylab.trace <- ylab }

    # 'xbreaks' argument
    if (isTRUE(class(xbreaks) == "waiver")) {

      if (isTRUE(burnin)) {

        xbreaks.trace <- sort(c(1L, seq(0L, n.postburn, length.out = 5L)[-1L], n.burnin))

      } else {

        xbreaks.trace <- seq(n.burnin, n.postburn, length.out = 5L)

      }

    } else {

      xbreaks.trace <- xbreaks

    }

    # 'ybreaks' argument
    ybreaks.trace <- ybreaks

    # 'xlim' argument
    if (isTRUE(is.null(xlim))) {

      if (isTRUE(burnin)) {

        xlim.trace <- c(0L, n.postburn)

      } else {

        xlim.trace <- c(n.burnin, n.postburn)

      }

    } else {

      xlim.trace <- xlim

    }

    # 'ylim' argument
    ylim.trace <- ylim

    # 'xexpand' argument
    if (isTRUE(class(xexpand) == "waiver")) { xexpand.trace <- c(0.02, 0L) } else { xexpand.trace <- xexpand }

    # 'yexpand' argument
    if (isTRUE(class(yexpand) == "waiver")) { yexpand.trace <- c(0.02, 0L) } else { yexpand.trace <- yexpand }

    # 'plot.margin' argument
    if (isTRUE(is.null(plot.margin))) { plot.margin.trace <- c(4L, 15L, -10L, 0L) } else { plot.margin.trace <- plot.margin }

    # 'legend.box.margin' argument
    if (isTRUE(is.null(legend.box.margin))) { legend.box.margin.trace <- c(-16L, 6L, 6L, 6L) } else { legend.box.margin.trace <- legend.box.margin }

    # Plot
    plot.trace <- suppressMessages(ggplot2::ggplot(plotdat.trace, ggplot2::aes(x = iter, y = value, color = chain)) +
                                     ggplot2::facet_wrap(~ param, nrow = nrow, ncol = ncol, scales = scales) +
                                     ggplot2::scale_x_continuous(name = xlab.trace, limits = xlim.trace, breaks = xbreaks.trace, expand = xexpand.trace) +
                                     ggplot2::scale_y_continuous(name = ylab.trace, limits = ylim.trace, breaks = ybreaks.trace, expand = yexpand.trace) +
                                     ggplot2::scale_colour_manual(name = "Chain", values = hcl.colors(n = n.chains, palette = palette)) +
                                     ggplot2::theme_bw() +
                                     ggplot2::guides(color = ggplot2::guide_legend(nrow = 1, byrow = TRUE)) +
                                     ggplot2::theme(plot.margin = ggplot2::margin(t = plot.margin.trace[1L], r = plot.margin.trace[2L], b = plot.margin.trace[3L], l = plot.margin.trace[4L]),
                                                    legend.position = "bottom",
                                                    legend.title = ggplot2::element_text(size = legend.title.size),
                                                    legend.text = ggplot2::element_text(size = legend.text.size),
                                                    legend.box.margin = ggplot2::margin(t = legend.box.margin.trace[1L], r = legend.box.margin.trace[2L], b = legend.box.margin.trace[3L], l = legend.box.margin.trace[4L]),
                                                    legend.background = ggplot2::element_rect(fill = "transparent")))

    # With burnin phase
    if (isTRUE(burnin)) {

      plot.trace <- plot.trace + ggplot2::annotate("rect", xmin = 0L, xmax = n.burnin, ymin = -Inf, ymax = Inf, alpha = alpha, fill = fill) +
        ggplot2::geom_line()

    # Without burnin phase
    } else {

      plot.trace <- plot.trace + ggplot2::geom_line()

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Posterior Distribution ####

    # 'xlab' argument
    if (isTRUE(is.null(xlab))) { xlab.post <- "" } else { xlab.post <- xlab }

    # 'ylab' argument
    if (isTRUE(is.null(ylab))) { ylab.post <- "Probability Density, f(x)" } else { ylab.post <- ylab }

    # 'xbreaks' argument
    xbreaks.post <- xbreaks

    # 'ybreaks' argument
    ybreaks.post <- ybreaks

    # 'xlim' argument
    xlim.post <- xlim

    # 'ylim' argument
    ylim.post <- ylim

    # 'xexpand' argument
    if (isTRUE(class(xexpand) == "waiver")) { xexpand.post <- c(0.02, 0L) } else { xexpand.post <- xexpand }

    # 'yexpand' argument
    if (isTRUE(class(yexpand) == "waiver")) { yexpand.post <- ggplot2::expansion(mult = c(0L, 0.05)) } else { yexpand.post <- yexpand }

    # 'plot.margin' argument
    if (isTRUE(is.null(plot.margin))) { plot.margin.post <- c(4L, 15L, -4L, 4L) } else { plot.margin.post <- plot.margin }

    # Plot
    plot.post <- suppressMessages(ggplot2::ggplot(plotdat.post, ggplot2::aes(x = value)) +
                                    ggplot2::facet_wrap(~ param, nrow = nrow, ncol = ncol, scales = scales) +
                                    ggplot2::scale_x_continuous(name = xlab.post, limits = xlim.post, breaks = xbreaks.post, expand = xexpand.post) +
                                    ggplot2::scale_y_continuous(name = ylab.post, limits = ylim.post, breaks = ybreaks.post, expand = yexpand.post) +
                                    ggplot2::theme_bw() +
                                    ggplot2::theme(plot.margin = ggplot2::margin(t = plot.margin.post[1L], r = plot.margin.post[2L], b = plot.margin.post[3L], l = plot.margin.post[4L])))

    # Histogram
    if (isTRUE(hist)) { plot.post <- suppressMessages(plot.post + ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), binwidth = binwidth, bins = bins, color = "black", alpha = alpha, fill = fill)) }

    # Density curve
    if (isTRUE(density)) { plot.post <- suppressMessages(plot.post + ggplot2::geom_density(color = density.col)) }

    #### Point Estimates ####

    if (isTRUE(all(point != "none"))) {

      # Posterior Mean
      if (isTRUE("m" %in% point)) {

        # 'plot.margin' argument
        if (isTRUE(is.null(plot.margin))) { plot.margin.post <- c(4L, 15L, -8L, 4L) } else { plot.margin.post <- plot.margin }

        # 'legend.box.margin' argument
        if (isTRUE(is.null(legend.box.margin))) { legend.box.margin.post <- c(-24L, 6L, 6L, 6L) } else { legend.box.margin.post <- legend.box.margin }

        plot.post <- suppressMessages(plot.post +
                                        ggplot2::geom_vline(data = data.frame(param = levels(plotdat.post$param), stat = tapply(plotdat.post$value, plotdat.post$param, mean, na.rm = TRUE)),
                                                            ggplot2::aes(xintercept = stat, color = "Mean"), linewidth = linewidth) +
                                        ggplot2::scale_color_manual(name = "Point Estimate", values = c(Mean = point.col[1L])) +
                                        ggplot2::guides(color = ggplot2::guide_legend(nrow = 1, byrow = TRUE)) +
                                        ggplot2::theme(plot.margin = ggplot2::margin(t = plot.margin.post[1L], r = plot.margin.post[2L], b = plot.margin.post[3L], l = plot.margin.post[4L]),
                                                       legend.position = "bottom",
                                                       legend.title = ggplot2::element_text(size = legend.title.size),
                                                       legend.text = ggplot2::element_text(size = legend.text.size),
                                                       legend.box.margin = ggplot2::margin(t = legend.box.margin.post[1L], r = legend.box.margin.post[2L], b = legend.box.margin.post[3L], l = legend.box.margin.post[4L]),
                                                       legend.background = ggplot2::element_rect(fill = "transparent")))

      }

      # Posterior Median
      if (isTRUE("med" %in% point)) {

        # 'plot.margin' argument
        if (isTRUE(is.null(plot.margin))) { plot.margin.post <- c(4L, 15L, -8L, 4L) } else { plot.margin.post <- plot.margin }

        # 'legend.box.margin' argument
        if (isTRUE(is.null(legend.box.margin))) { legend.box.margin.post <- c(-24L, 6L, 6L, 6L) } else { legend.box.margin.post <- legend.box.margin }

        plot.post <- suppressMessages(plot.post +
                                        ggplot2::geom_vline(data = data.frame(param = levels(plotdat.post$param), stat = tapply(plotdat.post$value, plotdat.post$param, median, na.rm = TRUE)),
                                                            ggplot2::aes(xintercept = stat, color = "Median"), linewidth = linewidth) +
                                        ggplot2::scale_color_manual(name = "Point Estimate", values = c(Mean = point.col[1L], Median = point.col[2L])) +
                                        ggplot2::guides(color = ggplot2::guide_legend(nrow = 1, byrow = TRUE)) +
                                        ggplot2::theme(plot.margin = ggplot2::margin(t = plot.margin.post[1L], r = plot.margin.post[2L], b = plot.margin.post[3L], l = plot.margin.post[4L]),
                                                       legend.position = "bottom",
                                                       legend.title = ggplot2::element_text(size = legend.title.size),
                                                       legend.text = ggplot2::element_text(size = legend.text.size),
                                                       legend.box.margin = ggplot2::margin(t = legend.box.margin.post[1L], r = legend.box.margin.post[2L], b = legend.box.margin.post[3L], l = legend.box.margin.post[4L]),
                                                       legend.background = ggplot2::element_rect(fill = "transparent")))

      }

      # Posterior Maximum A Posteriori
      if (isTRUE("map" %in% point)) {

        # 'plot.margin' argument
        if (isTRUE(is.null(plot.margin))) { plot.margin.post <- c(4L, 15L, -8L, 4L) } else { plot.margin.post <- plot.margin }

        # 'legend.box.margin' argument
        if (isTRUE(is.null(legend.box.margin))) { legend.box.margin.post <- c(-24L, 6L, 6L, 6L) } else { legend.box.margin.post <- legend.box.margin }

        plot.post <- suppressMessages(plot.post +
                                        ggplot2::geom_vline(data = data.frame(param = levels(plotdat.post$param), stat = tapply(plotdat.post$value, plotdat.post$param, function(y) {
                                          x.density <- density(na.omit(y), n = 2L^10L, bw = "SJ", from = range(y)[1L], to = range(y)[2L])
                                          x.density$x[which.max(x.density$y)]
                                        })),
                                        ggplot2::aes(xintercept = stat, color = "MAP"), linewidth = linewidth) +
                                        ggplot2::scale_color_manual(name = "Point Estimate", values = c(Mean = point.col[1L], Median = point.col[2L], MAP = point.col[3L])) +
                                        ggplot2::guides(color = ggplot2::guide_legend(nrow = 1, byrow = TRUE)) +
                                        ggplot2::theme(plot.margin = ggplot2::margin(t = plot.margin.post[1L], r = plot.margin.post[2L], b = plot.margin.post[3L], l = plot.margin.post[4L]),
                                                       legend.position = "bottom",
                                                       legend.title = ggplot2::element_text(size = legend.title.size),
                                                       legend.text = ggplot2::element_text(size = legend.text.size),
                                                       legend.box.margin = ggplot2::margin(t = legend.box.margin.post[1L], r = legend.box.margin.post[2L], b = legend.box.margin.post[3L], l = legend.box.margin.post[4L]),
                                                       legend.background = ggplot2::element_rect(fill = "transparent")))

      }

    }

    #### Credible Interval ####

    if (isTRUE(ci != "none")) {

      ##### Equal-Tailed Interval
      switch(ci, eti = {

        plot.post <- suppressMessages(plot.post +
                                        ggplot2::geom_vline(data = data.frame(param = levels(plotdat.post$param),
                                                                              low = tapply(plotdat.post$value, plotdat.post$param, function(y) quantile(y, probs = (1L - conf.level) / 2L))),
                                                            ggplot2::aes(xintercept = low), color = line.col, linetype = linetype, linewidth = linewidth) +
                                        ggplot2::geom_vline(data = data.frame(param = levels(plotdat.post$param),
                                                                              upp = tapply(plotdat.post$value, plotdat.post$param, function(y) quantile(y, probs = 1L - (1L - conf.level) / 2L))),
                                                            ggplot2::aes(xintercept = upp), color = line.col, linetype = linetype, linewidth = linewidth) +
                                        ggplot2::labs(caption = paste0(round(conf.level * 100, digits = 2), "% Equal-Tailed Interval")) +
                                        ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0.5, vjust = 7)))

      ##### Highest Density Interval
      }, hdi = {

        plot.post <- suppressMessages(plot.post +
                                        ggplot2::geom_vline(data = data.frame(param = levels(plotdat.post$param),
                                                                              low = tapply(plotdat.post$value, plotdat.post$param, function(y) .hdi(y, conf.level = conf.level)$low)),
                                                            ggplot2::aes(xintercept = low), color = line.col, linetype = linetype, linewidth = linewidth) +
                                        ggplot2::geom_vline(data = data.frame(param = levels(plotdat.post$param),
                                                                              upp = tapply(plotdat.post$value, plotdat.post$param, function(y) .hdi(y, conf.level = conf.level)$upp)),
                                                            ggplot2::aes(xintercept = upp), color = line.col, linetype = linetype, linewidth = linewidth) +
                                        ggplot2::labs(caption = paste0(round(conf.level * 100L, digits = 2L), "% Highest Density Interval")) +
                                        ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0.5, vjust = 7)))

      })

    }

    #_____________________________________________________________________________
    #
    # Return Object --------------------------------------------------------------

    object <- list(call = match.call(),
                   type = "blimp",
                   x = x,
                   args = list(plot = plot, param = param, burnin = burnin,
                               point = point, ci = ci, conf.level = conf.level,
                               hist = hist, density = density, area = area,
                               alpha = alpha, fill = fill, nrow = nrow, ncol = ncol,
                               scales = scales, xlab = xlab, ylab = ylab,
                               xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks,
                               xexpand = xexpand, yexpand = yexpand,
                               palette = palette, binwidth = binwidth, bins = bins,
                               density.col = density.col, shape = shape, point.col = point.col,
                               linewidth = linewidth, linetype = linetype, line.col = line.col,
                               plot.margin = plot.margin, legend.title.size = legend.title.size,
                               legend.text.size = legend.text.size, legend.box.margin = legend.box.margin,
                               saveplot = saveplot, file = file, file.plot = file.plot,
                               width = width, height = height, units = units, dpi = dpi,
                               check = check),
                   data = list(plotdat = plotdat, trace = plotdat.trace, post = plotdat.post),
                   plot = list(trace = plot.trace, post = plot.post))

    class(object) <- "misty.object"

  #----------------------------------------
  # Blimp Plots in misty object
  } else {

    x <- object

  }

  #_____________________________________________________________________________
  #
  # Plot -----------------------------------------------------------------------

  if (isTRUE(all(plot != "none"))) {

    switch(plot,
           # Trace plots
           trace = { suppressWarnings(suppressMessages(plot(object$plot$trace))) },
           # Posterior distribution plots
           post = { suppressWarnings(suppressMessages(plot(object$plot$post))) })

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(invisible(object))

}
