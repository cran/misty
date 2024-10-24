#' Plot Mplus GH5 File
#'
#' This function uses the \code{h5file} function in the \pkg{hdf5r} package to
#' read a Mplus GH5 file that is requested by the command \code{PLOT: TYPE IS PLOT2}
#' in Mplus to display trace plots, posterior distribution plots, autocorrelation
#' plots, posterior predictive check plots based on the "bayesian_data" section,
#' and the loop plot based on the "loop_data" section of the Mplus GH5 file. By
#' default, the function displays trace plots if the "bayesian_data" section is
#' available in the Mplus GH5 File. Otherwise, the function plots the loop plot
#' if the "loop_data" section is available in the Mplus GH5 file.
#'
#' @param x                 a character string indicating the name of the Mplus
#'                          GH5 file (HDF5 format) with or without the file
#'                          extension \code{.gh5}, e.g., \code{"Mplus_Plot.gh5"}
#'                          or \code{"Mplus_Plot"}. Alternatively, a \code{misty.object}
#'                          of type \code{mplus} can be specified, i.e., result
#'                          object of the \code{mplus.plot()} function.
#' @param plot              a character string indicating the type of plot to
#'                          display, i.e., \code{"none"} for not displaying any
#'                          plot, \code{"trace"} (default) for displaying trace
#'                          plots, \code{post} for displaying posterior distribution
#'                          plots, \code{"auto"} for displaying autocorrelation
#'                          plots, \code{"ppc"} for displaying posterior predictive
#'                          check plots, and \code{"loop"} for displaying the
#'                          loop plot. The default setting is \code{"trace"} if
#'                          the "bayesian_data" section is available in the Mplus
#'                          GH5 file. Otherwise, the default setting switches to
#'                          \code{"loop"}.
#' @param param             character vector indicating which parameters to print
#'                          for the trace plots, posterior distribution plots,
#'                          and autocorrelation plots, i.e., \code{"all"} for all
#'                          parameters, \code{"on"} (default), for regression
#'                          slopes, \code{"by"} for factor loadings, \code{"with"}
#'                          for covariances, \code{"inter"} for intercepts and
#'                          thresholds, \code{"var"} for (residual) variances,
#'                          \code{"r2"} for r-square, and \code{"new"} for
#'                          parameters not in the analysis model specified in the
#'                          \code{NEW} option. The default setting is \code{"on"}
#'                          if regression slopes are available. Otherwise, the
#'                          default setting switches to \code{"by"} and to
#'                          \code{"with"} if factor loadings are not available.
#' @param std               a character vector indicating the standardized
#'                          parameters to print for the trace plots, posterior
#'                          distribution plots, and autocorrelation plots, i.e.,
#'                          \code{"all"} for all standardized parameters,
#'                          \code{"none"} (default) for not printing any
#'                          standardized parameters, \code{"stdyx"} for StdYX
#'                          standardized parameters, \code{"stdy"} for StdY
#'                          standardized parameters, and \code{"std"} for StdX
#'                          standardized parameters.
#' @param burnin            logical: if \code{FALSE}, the first half of each chain
#'                          is discarded as being part of the burnin phase when
#'                          displaying trace plots. The default setting for
#'                          \code{plot = "trace"} is \code{TRUE}. Note that the
#'                          first half of each chain is always discarded when
#'                          displaying posterior distribution plots
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
#' @param chain             a numerical value indicating the chain to be used for
#'                          the autocorrelation plots. By default, the first
#'                          chain is used.
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
#'                          \code{geom_histogram}, \code{geom_bar}, and
#'                          \code{geom_ribbon} function.
#' @param fill              a character string indicating the color for the
#'                          \code{"fill"} argument (default is \code{"gray85"})
#'                          for the \code{annotate}, \code{geom_histogram},
#'                          \code{geom_bar}, and \code{geom_point} functions.
#' @param nrow              a numeric value indicating the \code{nrow} argument
#'                          (default is \code{NULL}) for the \code{facet_wrap}
#'                          function.
#' @param ncol              a numeric value indicating the \code{ncol} argument
#'                          (default is \code{2}) for the \code{facet_wrap} function.
#' @param scales            a character string indicating the \code{scales} argument
#'                          (default is \code{"free"}) for the \code{facet_wrap}
#'                          function.
#' @param xlab              a character string indicating the \code{name} argument
#'                          for the \code{scale_x_continuous} function. Note that
#'                          the default setting depends on the type of plot,
#'                          e.g., \code{""} for the trace plots and \code{"Lag"}
#'                          for the autocorrelation plots.
#' @param ylab              a character string indicating the \code{name} argument
#'                          for the \code{scale_y_continuous} function. Note that
#'                          the default setting depends on the type of plot,
#'                          e.g., \code{""} for the trace plots and \code{"Autocorrelation"}
#'                          for the autocorrelation plots.
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
#' @param bar.col           a character string indicating the \code{color} argument
#'                          (default is \code{"black"}) for the \code{geom_bar}
#'                          function.
#' @param bar.width         a character string indicating the \code{width} argument
#'                          (default = \code{0.8})for the \code{geom_bar} function.
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
#'                          for saving the trace plots, \code{post} for the saving
#'                          the posterior distribution plots, \code{"auto"} for
#'                          saving the autocorrelation plots, \code{"ppc"} for
#'                          saving the posterior predictive check plots, and
#'                          \code{"loop"} for saving the loop plot.
#' @param file              a character string indicating the \code{filename}
#'                          argument (default is \code{"Mplus_Plot.pdf"}) including
#'                          the file extension for the \code{ggsave} function.
#'                          Note that one of \code{".eps"}, \code{".ps"},
#'                          \code{".tex"}, \code{".pdf"} (default), \code{".jpeg"},
#'                          \code{".tiff"}, \code{".png"}, \code{".bmp"},
#'                          \code{".svg"} or \code{".wmf"} needs to be specified
#'                          as file extension in the \code{file} argument.
#' @param file.plot         a character vector with five elements for distinguishing
#'                          different types of plots. By default, the character
#'                          string specified in the argument \code{"file"}
#'                          (\code{"Mplus_Plot"}) is concatenated with \code{"_TRACE"}
#'                          (\code{"Mplus_Plot_TRACE"}) for the trace plots,
#'                          \code{"_POST"} (\code{"Mplus_Plot_POST"}) for
#'                          the posterior distribution plots, \code{"_AUTO"}
#'                          (\code{"Mplus_Plot_AUTO"}) for the autocorrelation
#'                          plots, \code{"_PPC"} (\code{"Mplus_Plot_PPC"}) for the
#'                          posterior predictive check plots, and \code{"_LOOP"}
#'                          (\code{"Mplus_Plot_LOOP"}) for the loop plot.
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
#' \code{\link{read.mplus}}, \code{\link{write.mplus}}, \code{\link{mplus}},
#' \code{\link{mplus.update}}, \code{\link{mplus.print}}, \code{\link{mplus.bayes}},
#' \code{\link{mplus.run}}, \code{\link{mplus.lca}}
#'
#' @references
#' Muthen, L. K., & Muthen, B. O. (1998-2017). \emph{Mplus User's Guide} (8th ed.).
#' Muthen & Muthen.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{x}}{Mplus GH5 file}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{data}}{list with posterior distribution of each parameter estimate
#'                    in wide and long format (\code{post}), autocorrelation for
#'                    each parameter estimate in wide and long format (\code{auto}),
#'                    data for the posterior predictive check (\code{ppc}),
#'                    and data for the loop plot (\code{loop})}
#' \item{\code{plot}}{list with the trace plots (\code{trace}, posterior distribution
#'                    plots (\code{post}), autocorrelation plots (\code{auto}),
#'                    posterior predictive check plots (\code{ppc}), and
#'                    loop plot (\code{loop})}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #----------------------------------------------------------------------------
#' # Mplus Example 3.18: Moderated Mediation with a Plot of the Indirect Effect
#'
#' #..........
#' # Trace Plots
#'
#' # Example 1a: Default setting
#' mplus.plot("ex3.18.gh5")
#'
#' # Example 1b: Exclude first half of each chain
#' mplus.plot("ex3.18.gh5", burnin = FALSE)
#'
#' # Example 1c: Print all parameters
#' mplus.plot("ex3.18.gh5", param = "all")
#'
#' # Example 1d: Print parameters not in the analysis model
#' mplus.plot("ex3.18.gh5", param = "new")
#'
#' # Example 1e: Arrange panels in three columns
#' mplus.plot("ex3.18.gh5", ncol = 3)
#'
#' # Example 1f: Specify "Pastel 1" palette for the hcl.colors function
#' mplus.plot("ex3.18.gh5", palette = "Pastel 1")
#'
#' #..........
#' # Posterior Distribution Plots
#'
#' # Example 2a: Default setting, i.e., posterior median and equal-tailed interval
#' mplus.plot("ex3.18.gh5", plot = "post")
#'
#' # Example 2b: Display posterior mean and maximum a posteriori
#' mplus.plot("ex3.18.gh5", plot = "post", point = c("m", "map"))
#'
#' # Example 2c: Display maximum a posteriori and highest density interval
#' mplus.plot("ex3.18.gh5", plot = "post", point = "map", ci = "hdi")
#'
#' # Example 2d: Do not display any point estimates and credible interval
#' mplus.plot("ex3.18.gh5", plot = "post", point = "none", ci = "none")
#'
#' # Example 2d: Do not display histograms
#' mplus.plot("ex3.18.gh5", plot = "post", hist = FALSE)
#'
#' #..........
#' # Autocorrelation Plots
#'
#' # Example 3a: Default setting, i.e., first chain
#' mplus.plot("ex3.18.gh5", plot = "auto")
#'
#' # Example 3b: Use second chain
#' mplus.plot("ex3.18.gh5", plot = "auto", chain = 2)
#'
#' # Example 3b: Modify limits and breaks of the y-axis
#' mplus.plot("ex3.18.gh5", plot = "auto",
#'            ylim = c(-0.05, 0.05), ybreaks = seq(-0.1, 0.1, by = 0.025))
#'
#' #..........
#' # Posterior Predictive Check Plots
#'
#' # Example 4a: Default setting, i.e., 95% Interval
#' mplus.plot("ex3.18.gh5", plot = "ppc")
#'
#' # Example 4b: Default setting, i.e., 99% Interval
#' mplus.plot("ex3.18.gh5", plot = "ppc", conf.level = 0.99)
#'
#' #..........
#' # Loop Plot
#'
#' # Example 5a: Default setting
#' mplus.plot("ex3.18.gh5", plot = "loop")
#'
#' # Example 5b: Do not fill area and draw vertical lines
#' mplus.plot("ex3.18.gh5", plot = "loop", area = FALSE)
#'
#' #..........
#' # Save Plots
#'
#' # Example 6a: Save all plots in pdf format
#' mplus.plot("ex3.18.gh5", saveplot = "all")
#'
#' # Example 6b: Save all plots in png format with 300 dpi
#' mplus.plot("ex3.18.gh5", saveplot = "all", file = "Mplus_Plot.png", dpi = 300)
#'
#' # Example 6a: Save loop plot, specify width and height of the plot
#' mplus.plot("ex3.18.gh5", plot = "none", saveplot = "loop",
#'            width = 7.5, height = 7)
#'
#' #----------------------------------------------------------------------------
#' # Plot from misty.object
#'
#' # Create misty.object
#' object <- mplus.plot("ex3.18.gh5", plot = "none")
#'
#' # Trace plot
#' mplus.plot(object, plot = "trace")
#'
#' # Posterior distribution plot
#' mplus.plot(object, plot = "post")
#'
#' # Autocorrelation plot
#' mplus.plot(object, plot = "auto")
#'
#' # Posterior predictive check plot
#' mplus.plot(object, plot = "ppc")
#'
#' # Loop plot
#' mplus.plot(object, plot = "loop")
#'
#' #----------------------------------------------------------------------------
#' # Create Plots Manually
#'
#' # Load ggplot2 package
#' library(ggplot2)
#'
#' # Create misty object
#' object <- mplus.plot("ex3.18.gh5", plot = "none")
#'
#' #..........
#' # Example 7: Trace Plots
#'
#' # Extract data in long format
#' data.post <- object$data$post$long
#'
#' # Extract ON parameters
#' data.trace <- data.post[grep(" ON ", data.post$param), ]
#'
#' # Plot
#' ggplot(data.trace, aes(x = iter, y = value, color = chain)) +
#'   annotate("rect", xmin = 0, xmax = 15000, ymin = -Inf, ymax = Inf,
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
#' # Example 8: Posterior Distribution Plots
#'
#' # Extract data in long format
#' data.post <- object$data$post$long
#'
#' # Extract ON parameters
#' data.post <- data.post[grep(" ON ", data.post$param), ]
#'
#' # Discard burn-in iterations
#' data.post <- data.post[data.post$iter > 15000, ]
#'
#' # Drop factor levels
#' data.post$param <- droplevels(data.post$param,
#'                               exclude = c("[Y]", "[M]", "Y", "M", "INDIRECT", "MOD"))
#'
#' # Plot
#' ggplot(data.post, aes(x = value)) +
#'   geom_histogram(aes(y = after_stat(density)), color = "black", alpha = 0.4,
#'                  fill = "gray85") +
#'   geom_density(color = "#0072B2") +
#'   geom_vline(data = data.frame(param = unique(data.post$param),
#'                                stat = tapply(data.post$value, data.post$param, median)),
#'              aes(xintercept = stat, color = "Median"), linewidth = 0.6) +
#'   geom_vline(data = data.frame(param = unique(data.post$param),
#'                                low = tapply(data.post$value, data.post$param,
#'                                             function(y) quantile(y, probs = 0.025))),
#'              aes(xintercept = low), linetype = "dashed", linewidth = 0.6) +
#'   geom_vline(data = data.frame(param = unique(data.post$param),
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
#'
#' #..........
#' # Example 9: Autocorrelation Plots
#'
#' # Extract data in long format
#' data.auto <- object$data$auto$long
#'
#' # Select first chain
#' data.auto <- data.auto[data.auto$chain == 1, ]
#'
#' # Extract ON parameters
#' data.auto <- data.auto[grep(" ON ", data.auto$param), ]
#'
#' # Plot
#' ggplot(data.auto, aes(x = lag, y = cor)) +
#'   geom_bar(stat = "identity", alpha = 0.4, color = "black", fill = "gray85",
#'            width = 0.8) +
#'   facet_wrap(~ param, ncol = 2) +
#'   scale_x_continuous(name = "Lag", breaks = seq(1, 30, by = 2), expand = c(0.02, 0)) +
#'   scale_y_continuous(name = "Autocorrelation", limits = c(-0.1, 0.1),
#'                      breaks = seq(-0.1, 1., by = 0.05), expand = c(0.02, 0)) +
#'   theme_bw() +
#'   theme(plot.margin = margin(c(4, 15, 4, 4)))
#'
#' #..........
#' # Example 10: Posterior Predictive Check (PPC) Plots
#'
#' # Extract data
#' data.ppc <- object$data$ppc
#'
#' # Scatter plot
#' ppc.scatter <- ggplot(data.ppc, aes(x = obs, y = rep)) +
#'   geom_point(shape = 21, fill = "gray85") +
#'   geom_abline(slope = 1) +
#'   scale_x_continuous("Observed", limits = c(0, 45), breaks = seq(0, 45, by = 5),
#'                      expand = c(0.02, 0)) +
#'   scale_y_continuous("Recpliated", limits = c(0, 45), breaks = seq(0, 45, by = 5),
#'                      expand = c(0.02, 0)) +
#'   theme_bw() +
#'   theme(plot.margin = margin(c(2, 15, 4, 4)))
#'
#' # Histogram
#' ppc.hist <- ggplot(data.ppc, aes(x = diff)) +
#'   geom_histogram(color = "black", alpha = 0.4, fill = "gray85") +
#'   geom_vline(xintercept = mean(data.ppc$diff), color = "#CC79A7") +
#'   geom_vline(xintercept = quantile(data.ppc$diff, probs = 0.025),
#'              linetype = "dashed", color = "#CC79A7") +
#'   geom_vline(xintercept = quantile(data.ppc$diff, probs = 0.975),
#'              linetype = "dashed", color = "#CC79A7") +
#'   scale_x_continuous("Observed - Replicated", expand = c(0.02, 0)) +
#'   scale_y_continuous("Count", expand = expansion(mult = c(0L, 0.05))) +
#'   theme_bw() +
#'   theme(plot.margin = margin(c(2, 15, 4, 4)))
#'
#' # Combine plots using the patchwork package
#' patchwork::wrap_plots(ppc.scatter, ppc.hist)
#'
#' #..........
#' # Example 11: Loop Plot
#'
#' # Extract data
#' data.loop <- object$data$loop
#'
#' # Plot
#' plot.loop <- ggplot(data.loop, aes(x = xval, y = estimate)) +
#'   geom_line(linewidth = 0.6, show.legend = FALSE) +
#'   geom_line(aes(xval, low)) +
#'   geom_line(aes(xval, upp)) +
#'   scale_x_continuous("MOD", expand = c(0.02, 0)) +
#'   scale_y_continuous("INDIRECT", expand = c(0.02, 0)) +
#'   scale_fill_manual("Statistical Significance",
#'                     values = hcl.colors(n = 2, palette = "Set 2")) +
#'   theme_bw() +
#'   theme(plot.margin = margin(c(4, 15, -6, 4)),
#'         legend.position = "bottom",
#'         legend.title = element_text(size = 10),
#'         legend.text = element_text(size = 10),
#'         legend.box.margin = margin(-10, 6, 6, 6),
#'         legend.background = element_rect(fill = "transparent"))
#'
#' # Significance area
#' for (i in unique(data.loop$group)) {
#'
#'   plot.loop <- plot.loop + geom_ribbon(data = data.loop[data.loop$group == i, ],
#'                                        aes(ymin = low, ymax = upp, fill = sig), alpha = 0.4)
#'
#' }
#'
#' # Vertical lines
#' plot.loop + geom_vline(data = data.loop[data.loop$change == 1, ],
#'                        aes(xintercept = xval, color = sig), linewidth = 0.6,
#'                            linetype = "dashed", show.legend = FALSE)
#' }
mplus.plot <- function(x, plot = c("none", "trace", "post", "auto", "ppc", "loop"),
                       param = c("all", "on", "by", "with", "inter", "var", "r2", "new"),
                       std = c("all", "none", "stdyx", "stdy", "std"), burnin = TRUE,
                       point = c("all", "none", "m", "med", "map"),
                       ci = c("none", "eti", "hdi"), chain = 1, conf.level = 0.95,
                       hist = TRUE, density = TRUE, area = TRUE,
                       alpha = 0.4, fill = "gray85", nrow = NULL, ncol = NULL,
                       scales = c("fixed", "free", "free_x", "free_y"),
                       xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL,
                       xbreaks = ggplot2::waiver(), ybreaks = ggplot2::waiver(),
                       xexpand = ggplot2::waiver(), yexpand = ggplot2::waiver(),
                       palette = "Set 2", binwidth = NULL, bins = NULL,
                       density.col = "#0072B2", shape = 21,
                       point.col = c("#CC79A7", "#D55E00", "#009E73"),
                       linewidth = 0.6, linetype = "dashed",
                       line.col = "black", bar.col = "black", bar.width = 0.8,
                       plot.margin = NULL, legend.title.size = 10, legend.text.size = 10,
                       legend.box.margin = NULL,
                       saveplot = c("all", "none", "trace", "post", "auto", "ppc", "loop"),
                       file = "Mplus_Plot.pdf", file.plot = c("_TRACE", "_POST", "_AUTO", "_PPC", "_LOOP"),
                       width = NA, height = NA, units = c("in", "cm", "mm", "px"),
                       dpi = 600, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check input 'x'
  if (isTRUE(missing(x))) { stop("Please specify a character string indicating the name of a Mplus GH5 file for the argument 'x'", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Chracter string ####

  if (isTRUE(is.character(x))) {

    # Character string
    if (isTRUE(length(x) != 1L)) { stop("Please specify a character string indicating the name of a Mplus GH5 file for the argument 'x'", call. = FALSE) }

    # File extension .gh5
    x <- ifelse(isTRUE(!grepl(".gh5", x)), file <- paste0(x, ".gh5"), x)

    # Check if 'x' exists
    if (isTRUE(!file.exists(x))) { stop(paste0("Unable to read the Mplus GH5 file: ", sQuote(x), " does not exist."), call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## misty object ####

  } else if (isTRUE(inherits(x, "misty.object"))) {

    if (isTRUE(x$type != "mplus")) { stop("Please specify a \"mplus\" object for the argument 'x'.", call. = FALSE) }

  } else {

    stop("Please specify a \"misty.object\" object or a character string indicating the name of a Mplus output file for the argument 'x'", call. = FALSE)

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # R package hdf5r
    if (isTRUE(!nzchar(system.file(package = "hdf5r")))) { stop("Package \"hdf5r\" is needed for this function, please install the package.", call. = FALSE) }

    # R package ggplot2
    if (isTRUE(!nzchar(system.file(package = "ggplot2")))) { stop("Package \"ggplot2\" is needed for this function, please install the package.", call. = FALSE) }

    # Check input 'plot'
    if (isTRUE(!all(plot %in% c("none", "trace", "post", "auto", "ppc", "loop")))) { stop("Character string in the argument 'plot' does not match with \"none\", \"trace\", \"post\", \"auto\", \"ppc\", or \"loop\".", call. = FALSE) }

    if (isTRUE(!all(c("none", "trace", "post", "auto", "ppc", "loop") %in% plot) && length(plot) != 1L)) { stop("Please specify \"none\", \"trace\", \"post\", \"auto\", \"ppc\", or \"loop\" for the argument 'plot'.", call. = FALSE) }

    # Check input 'param'
    if (isTRUE(!all(param %in% c("all", "on", "by", "with", "inter", "var", "r2", "new")))) { stop("Character strings in the argument 'param' do not all match with \"all\", \"on\", \"by\", \"with\", \"inter\", \"var\", \"r2\", or \"new\".", call. = FALSE) }

    # Check input 'std'
    if (isTRUE(!all(std %in% c("all", "none", "stdyx", "stdy", "std")))) { stop("Character strings in the argument 'std' do not all match with \"all\", \"none\", \"stdyx\", \"stdy\", or \"std\".", call. = FALSE) }

    # Check input 'burnin'
    if (isTRUE(!is.logical(burnin))) { stop("Please specify TRUE or FALSE for the argument 'burnin'.", call. = FALSE) }

    # Check input 'point'
    if (isTRUE(!all(point %in% c("all", "none", "m", "med", "map")))) { stop("Character strings in the argument 'point' do not all match with \"all\", \"none\", \"m\", \"med\", or \"map\".", call. = FALSE) }

    # Check input 'ci'
    if (isTRUE(!all(ci %in% c("none", "eti", "hdi")))) { stop("Character string in the argument 'ci' does not match with \"none\", \"eti\", or \"hdi\".", call. = FALSE) }

    if (isTRUE(!all(c("none", "eti", "hdi") %in% ci) && length(ci) != 1L)) { stop("Please specify \"none\", \"eti\", or \"hdi\" for the argument 'ci'.", call. = FALSE) }

    # Check input 'chain'
    if (isTRUE(chain %% 1L != 0L || chain < 0L)) { stop("Specify a positive integer number for the argument 'chain'.", call. = FALSE) }

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
    if (isTRUE(!all(saveplot %in% c("all", "none", "trace", "post", "auto", "ppc", "loop")))) { stop("Character strings in the argument 'saveplot' do not all match with \"all\", \"none\", \"trace\", \"post\", \"ppc\", or \"loop\".", call. = FALSE) }

    # Check input 'file.plot'
    if (isTRUE(length(file.plot) != 5L)) { stop("Please specify a character vector with five elements for the argument 'file.plot'.", call. = FALSE) }

    # Check input 'units'
    if (isTRUE(!all(units %in% c("in", "cm", "mm", "px")))) { stop("Character string in the argument 'units' does not match with \"in\", \"cm\", \"mm\", \"pdf\", or \"px\".", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'plot' Argument ####

  # Default setting
  if (isTRUE(all(c("none", "trace", "post", "auto", "ppc", "loop") %in% plot))) { plot <- "trace" }

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
  ## 'std' Argument ####

  # Default setting
  if (isTRUE(all(c("all", "none", "stdyx", "stdy", "std") %in% std))) {

    std <- "none"

  # All input commands
  } else if (isTRUE("all" %in% std)) {

    std <- c("stdyx", "stdy", "std")

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
  if (isTRUE(all(c("all", "none", "trace", "post", "auto", "ppc", "loop") %in% saveplot))) {

    saveplot <- "none"

  } else if (isTRUE("all" %in% saveplot)) {

    saveplot <- c("trace", "post", "auto", "ppc", "loop")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'units' Argument ####

  # Default setting
  if (isTRUE(all(c("in", "cm", "mm", "px") %in% units))) { units <- "in" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Global Variables ####

  estimate <- iter <- low <- obs <- sig <- stat <- upp <- value <- xval <- NULL

  #----------------------------------------
  # Mplus Plot Data in GH5 File
  if (isTRUE(!inherits(x, "misty.object"))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Read Mplus GH5 File ####

    gh5 <- tryCatch(hdf5r::h5file(x), error = function(y) { stop("Reading Mplus GH5 file using the h5file() function from the hdf5r package failed.", call. = FALSE) })

    # "bayesian_data" or "loop_data" section
    if (isTRUE(all(!c("bayesian_data", "loop_data") %in% names(gh5)))) { stop("There is no \"bayesian_data\" or \"loop_data\" section in the Mplus GH5 file specified in the argument 'x'", call. = FALSE) }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## "bayesian_data" section ####

    if (isTRUE("bayesian_data" %in% names(gh5))) {

      #...................
      ### Extract Posterior Data, Labels, and Dimensionality ####

      # Posterior
      post <- gh5[["bayesian_data/parameters_autocorr/parameters"]][, , ]

      # Parameter Labels
      labels <- gh5[["bayesian_data/parameters_autocorr/statements"]][] |>
        misty::chr.trim() |>
        misty::chr.gsub(pattern = c("^Parameter [0-9]+, ", "\\[ ", " \\]", "  "), replacement = c("", "\\[", "\\]", " "))

      # Dimensionality
      post.dim <- dim(post)

      # Number of parameters
      n.parameters <- post.dim[1L]

      # Number of iterations
      n.iterations <- post.dim[2L]

      # Number of chains
      n.chains <- post.dim[3L]

      #...................
      ### Posterior in Wide Format ####

      # Data
      post.wide <- data.frame(chain = rep(seq_len(n.chains), each = n.iterations), do.call("rbind", lapply(apply(post, 3L, function(y) setNames(as.data.frame(t(y)), nm = labels)), function(z) data.frame(iter = seq_len(n.iterations), z, check.names = FALSE))), check.names = FALSE)

      #...................
      ### Posterior in Long Format ####

      post.long <- subset(setNames(reshape(post.wide, varying = list(names(post.wide[-c(1L, 2L)])),
                                           times = names(post.wide[-c(1L, 2L)]), v.names = "value",
                                           direction = "long"), c("chain", "iter", "param", "value", "id")), select = c("chain", "iter", "param", "value"))

      # Factors
      post.long$param <- factor(post.long$param, levels = names(post.wide[-c(1, 2)]))
      post.long$chain <- factor(post.long$chain)

      # Row names
      row.names(post.long) <- NULL

      #...................
      ### Select Parameters ####

      #### Default or User-Specified Setting Setting: ON ####
      if (isTRUE(all(param == "on"))) {

        # ON parameter not available
        if (isTRUE(all(!grepl(" ON ", names(post.wide))))) {

          param <- "by"

          # BY parameter not available
          if (isTRUE(all(!grepl(" BY ", names(post.wide))))) {

            param <- "with"

            # WITH parameter not available
            if (isTRUE(all(!grepl(" WITH ", names(post.wide))))) {

              stop("There are no 'ON', 'BY', or 'WITH' parameters available in the Mplus GH5 file.", call. = FALSE)

            } else {

              message("There are no 'ON' or 'BY' parameters available in the Mplus GH5 file, param argument switched to \"with\".")

            }

          } else {

            message("There are no 'ON' parameters available in the Mplus GH5 file, param argument switched to \"by\".")

          }

        }

      #### User-Specified Setting: BY ####
      } else if (isTRUE(all(param == "by") && all(!grepl(" BY ", names(post.wide))))) {

        param <- "with"

        # WITH parameter not available
        if (isTRUE(all(!grepl(" WITH ", names(post.wide))))) {

          stop("There are no 'BY', or 'WITH' parameters available in the Mplus GH5 file.", call. = FALSE)

        } else {

          message("There are no 'BY' parameters available in the Mplus GH5 file, param argument switched to \"with\".")

        }

      #### User-Specified Setting: WITH ####
      } else if (isTRUE(all(param == "with") && all(!grepl(" WITH ", names(post.wide))))) {

        stop("There are no 'WITH' parameters available in the Mplus GH5 file.", call. = FALSE)

      }

      #### Extract Variables involved in BY, WITH, ON, [, $, or # ####

      var <- unname(unique(unlist(sapply(misty::chr.grep(c(" BY ", " WITH ", " ON ", "\\[", "\\]", "\\$"), names(post.wide), value = TRUE), function(y) {

        strsplit(unlist(strsplit(unlist(strsplit(misty::chr.gsub(c("\\[", "\\]", paste0("#", 1L:20L), paste0("$", 1L:20L), paste0("&", 1L:20L)), rep("", times = 62L), y), " ON ")), " BY ")), " WITH ")

      }))))

      # % statement
      if (isTRUE(any(grepl("%", var)))) {

        var <- unique(misty::chr.trim(sub(":", "", apply(rbind(var, unlist(lapply(strsplit(var, ""), function(y) grep(":", y)[1L])), nchar(var)), 2L, function(z) if (isTRUE(!is.na(z[2L]))) { substr(z[1L], start = z[2L], z[3L]) } else { z[1L]} )))) |>
                 (\(x) misty::chr.grep(paste0(": ", x, " "), misty::chr.grep(paste0(": ", x), names(post.wide), value = TRUE, fixed = TRUE), invert = TRUE, value = TRUE))()

      }

      #### Select Parameters ####

      # Select ON, BY, WITH, intercept/threshold
      plot.ind <- misty::chr.grep(misty::rec(misty::chr.omit(param, omit = "var", check = FALSE), spec = "'on' = ' ON '; 'by' = ' BY '; 'with' = ' WITH '; 'inter' = '['; 'r2' = 'R-SQUARE'"), names(post.wide), fixed = TRUE)

      # Select (residual) variance
      if (isTRUE("var" %in% param)) { plot.ind <- c(plot.ind, which(names(post.wide) %in% intersect(misty::chr.grep(c(" ON ", " BY ", " WITH ", "[", "R-SQUARE"), names(post.wide), fixed = TRUE, invert = TRUE, value = TRUE), var))) }

      # Select additional parameter
      if (isTRUE("new" %in% param)) { plot.ind <- c(plot.ind, which(names(post.wide) %in% misty::chr.omit(setdiff(misty::chr.grep(c(" ON ", " BY ", " WITH ", "[", "R-SQUARE"), names(post.wide), fixed = TRUE, invert = TRUE, value = TRUE), var), omit = c("chain", "iter")))) }

      # Exclude standardized
      if (isTRUE(std == "none")) {

        plot.ind <- setdiff(plot.ind, misty::chr.grep(c("STDYX,", "STDY,", "STD,"), names(post.wide), fixed = TRUE))

      } else {

        std.exclude <- setdiff(c("stdyx", "stdy", "std"), std)

        if (isTRUE(length(std.exclude) != 0L)) {

          plot.ind <- setdiff(plot.ind, misty::chr.grep(misty::rec(std.exclude, spec = "'stdyx' = 'STDYX,'; 'stdy' = 'STDY,'; 'std' = 'STD,'"), names(post.wide), fixed = TRUE))

        }

      }

      # No parameter selected
      if (isTRUE(length(plot.ind) == 0L)) { stop("There are no parameters selected for the trace, posterior distribution, or autocorrelation plots.", call. = FALSE) }

      # Include chain and iter
      plot.ind <- unique(sort(union(misty::chr.grep(c("chain", "iter"), names(post.wide)), plot.ind)))

      #...........................................................................
      ### Trace Plots ####

      # Select Parameters
      post.wide.plot <- post.wide[, plot.ind]

      # Plot Data
      plotdat.trace <- subset(setNames(reshape(post.wide.plot, varying = list(names(post.wide.plot[-c(1L, 2L)])),
                                               times = names(post.wide.plot[-c(1L, 2L)]), v.names = "value",
                                               direction = "long"), c("chain", "iter", "param", "value", "id")), select = c("chain", "iter", "param", "value"))

      # Factors
      plotdat.trace$param <- factor(plotdat.trace$param, levels = names(post.wide.plot[-c(1L, 2L)]))
      plotdat.trace$chain <- factor(plotdat.trace$chain)

      # Discard burn-in iterations
      if (isTRUE(!burnin)) {

        plotdat.trace <- plotdat.trace[which(!plotdat.trace$iter %in% seq_len(n.iterations / 2L)), ]

      }

      # 'xlab' argument
      if (isTRUE(is.null(xlab))) { xlab.trace <- "Iteration" } else { xlab.trace <- xlab }

      # 'ylab' argument
      if (isTRUE(is.null(ylab))) { ylab.trace <- "" } else { ylab.trace <- ylab }

      # 'xbreaks' argument
      if (isTRUE(inherits(xbreaks, "waiver"))) {

        if (isTRUE(burnin)) {

          xbreaks.trace <- sort(c(1L, seq(0L, n.iterations, length.out = 5L)[-1L], round(n.iterations / 2L)))

        } else {

          xbreaks.trace <- seq(round(n.iterations / 2L), n.iterations, length.out = 5L)

        }

      } else {

        xbreaks.trace <- xbreaks

      }

      # 'ybreaks' argument
      ybreaks.trace <- ybreaks

      # 'xlim' argument
      if (isTRUE(is.null(xlim))) { if (isTRUE(!burnin)) { xlim.trace <- c(n.iterations / 2L, n.iterations) } else { xlim.trace <- c(0L, n.iterations) } } else { xlim.trace <- xlim }

      # 'ylim' argument
      ylim.trace <- ylim

      # 'xexpand' argument
      if (isTRUE(inherits(xexpand, "waiver"))) { xexpand.trace <- c(0.02, 0L) } else { xexpand.trace <- xexpand }

      # 'yexpand' argument
      if (isTRUE(inherits(yexpand, "waiver"))) { yexpand.trace <- c(0.02, 0L) } else { yexpand.trace <- yexpand }

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

        plot.trace <- plot.trace + ggplot2::annotate("rect", xmin = 0L, xmax = round(n.iterations / 2L), ymin = -Inf, ymax = Inf, alpha = alpha, fill = fill) +
          ggplot2::geom_line()

      # Without burnin phase
      } else {

        plot.trace <- plot.trace + ggplot2::geom_line()

      }

      #...........................................................................
      ### Posterior Distribution ####

      # Discard burn-in iterations
      if (isTRUE(burnin)) {

        plotdat.post <- plotdat.trace[which(!plotdat.trace$iter %in% seq_len(n.iterations / 2L)), ]

      } else {

        plotdat.post <- plotdat.trace

      }

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
      if (isTRUE(inherits(xexpand, "waiver"))) { xexpand.post <- c(0.02, 0L) } else { xexpand.post <- xexpand }

      # 'yexpand' argument
      if (isTRUE(inherits(yexpand, "waiver"))) { yexpand.post <- ggplot2::expansion(mult = c(0L, 0.05)) } else { yexpand.post <- yexpand }

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
                         ggplot2::geom_vline(data = data.frame(param = unique(plotdat.post$param), stat = tapply(plotdat.post$value, plotdat.post$param, mean, na.rm = TRUE)),
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
                         ggplot2::geom_vline(data = data.frame(param = unique(plotdat.post$param), stat = tapply(plotdat.post$value, plotdat.post$param, median, na.rm = TRUE)),
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
                         ggplot2::geom_vline(data = data.frame(param = unique(plotdat.post$param), stat = tapply(plotdat.post$value, plotdat.post$param, function(y) {
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
                         ggplot2::geom_vline(data = data.frame(param = unique(plotdat.post$param),
                                                               low = tapply(plotdat.post$value, plotdat.post$param, function(y) quantile(y, probs = (1L - conf.level) / 2L))),
                                             ggplot2::aes(xintercept = low), color = line.col, linetype = linetype, linewidth = linewidth) +
                         ggplot2::geom_vline(data = data.frame(param = unique(plotdat.post$param),
                                                               upp = tapply(plotdat.post$value, plotdat.post$param, function(y) quantile(y, probs = 1L - (1L - conf.level) / 2L))),
                                             ggplot2::aes(xintercept = upp), color = line.col, linetype = linetype, linewidth = linewidth) +
                         ggplot2::labs(caption = paste0(round(conf.level * 100, digits = 2), "% Equal-Tailed Interval")) +
                         ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0.5, vjust = 7)))

        ##### Highest Density Interval
        }, hdi = {

          plot.post <- suppressMessages(plot.post +
                         ggplot2::geom_vline(data = data.frame(param = unique(plotdat.post$param),
                                                               low = tapply(plotdat.post$value, plotdat.post$param, function(y) .hdi(y, conf.level = conf.level)$low)),
                                             ggplot2::aes(xintercept = low), color = line.col, linetype = linetype, linewidth = linewidth) +
                         ggplot2::geom_vline(data = data.frame(param = unique(plotdat.post$param),
                                                               upp = tapply(plotdat.post$value, plotdat.post$param, function(y) .hdi(y, conf.level = conf.level)$upp)),
                                             ggplot2::aes(xintercept = upp), color = line.col, linetype = linetype, linewidth = linewidth) +
                         ggplot2::labs(caption = paste0(round(conf.level * 100L, digits = 2L), "% Highest Density Interval")) +
                         ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0.5, vjust = 7)))

        })

      }

      #...................
      ### Extract Autocorrelation Data ####

      # Autocorrelation
      auto <- gh5[["bayesian_data/parameters_autocorr/autocorrelation"]][, , ]

      # Dimensionarity
      auto.dim <- dim(auto)

      # Number of lags
      n.lag <- auto.dim[1L]

      # Number of parameters
      n.parameters <- auto.dim[2L]

      # Number of chains
      n.chains <- auto.dim[3L]

      #...................
      ### Autocorrelation Data in Wide Format ####

      auto.wide <- data.frame(chain = rep(seq_len(n.chains), each = n.lag), lag = seq_len(n.lag), do.call("rbind", apply(auto, 3L, function(y) setNames(as.data.frame(y), nm = labels))), check.names = FALSE)

      #...................
      ### Autocorrelation Data in Long Format ####

      auto.long <- subset(setNames(reshape(auto.wide, varying = list(names(auto.wide[-c(1L, 2L)])),
                                           times = names(auto.wide[-c(1L, 2L)]), v.names = "value",
                                           direction = "long"), c("chain", "lag", "param", "cor", "id")), select = c("chain", "lag", "param", "cor"))

      # Factors
      auto.long$param <- factor(auto.long$param, levels = names(auto.wide[-c(1, 2)]))
      auto.long$chain <- factor(auto.long$chain)

      # Row names
      row.names(auto.long) <- NULL

      #...........................................................................
      ### Autocorrelation Plot ####

      # Select Parameters
      auto.wide.plot <- auto.wide[, plot.ind]

      # Plot Data
      plotdat.auto <- subset(setNames(reshape(auto.wide.plot, varying = list(names(auto.wide.plot[-c(1L, 2L)])),
                                              times = names(auto.wide.plot[-c(1L, 2L)]), v.names = "value",
                                              direction = "long"), c("chain", "lag", "param", "cor", "id")), select = c("chain", "lag", "param", "cor"))

      # Factors
      plotdat.auto$param <- factor(plotdat.auto$param, levels = names(auto.wide.plot[-c(1, 2)]))
      plotdat.auto$chain <- factor(plotdat.auto$chain)

      # Select chain
      if (isTRUE(!chain %in% seq_len(n.chains))) { stop(paste0("Chain number ", chain, " is not available in the Mplus GH5 file specified in the argument 'x'."), call. = FALSE) }

      plotdat.auto <- plotdat.auto[which(plotdat.auto$chain == chain), ]

      # 'xlab' argument
      if (isTRUE(is.null(xlab))) { xlab.auto <- "Lag" } else { xlab.auto <- xlab }

      # 'ylab' argument
      if (isTRUE(is.null(ylab))) { ylab.auto <- "Autocorrelation"  } else { ylab.auto <- ylab }

      # 'xbreaks' argument
      if (isTRUE(inherits(xbreaks, "waiver"))) { xbreaks.auto <- seq(1L, 30L, by = 2L) } else { xbreaks.auto <- xbreaks }

      # 'ybreaks' argument
      if (isTRUE(inherits(ybreaks, "waiver"))) { ybreaks.auto <- seq(-1L, 1L, by = 0.25) } else { ybreaks.auto <- ybreaks }

      # 'xlim' argument
      if (isTRUE(is.null(xlim))) { xlim.auto <- c(0L, 31) } else { xlim.auto <- xlim }

      # 'ylim' argument
      if (isTRUE(is.null(ylim))) { ylim.auto <- c(-1L, 1L) } else { ylim.auto <- ylim }

      # 'xexpand' argument
      if (isTRUE(inherits(xexpand, "waiver"))) { xexpand.auto <- c(0.02, 0L) } else { xexpand.auto <- xexpand }

      # 'yexpand' argument
      if (isTRUE(inherits(yexpand, "waiver"))) { yexpand.auto <- c(0.02, 0L) } else { yexpand.auto <- yexpand }

      # 'plot.margin' argument
      if (isTRUE(is.null(plot.margin))) { plot.margin.auto <- c(4L, 15L, 4L, 4L) } else { plot.margin.auto <- plot.margin }

      # Plot
      plot.auto <- suppressMessages(ggplot2::ggplot(plotdat.auto, ggplot2::aes(x = lag, y = cor)) +
                     ggplot2::geom_bar(stat = "identity", color = bar.col, alpha = alpha, fill = fill, width = bar.width) +
                     ggplot2::facet_wrap(~ param, nrow = nrow, ncol = ncol, scales = scales) +
                     ggplot2::scale_x_continuous(name = xlab.auto, limits = xlim.auto, breaks = xbreaks.auto, expand = xexpand.auto) +
                     ggplot2::scale_y_continuous(name = ylab.auto, limits = ylim.auto, breaks = ybreaks.auto, expand = yexpand.auto) +
                     ggplot2::theme_bw() +
                     ggplot2::theme(plot.margin = ggplot2::margin(t = plot.margin.auto[1L], r = plot.margin.auto[2L], b = plot.margin.auto[3L], l = plot.margin.auto[4L])))

      #...................
      ### Posterior Predictive Check ####

      if (isTRUE("predictive" %in% names(gh5[["bayesian_data"]]))) {

        # R package patchwork
        if (isTRUE(!nzchar(system.file(package = "patchwork")))) { stop("Package \"patchwork\" is needed for this function, please install the package.", call. = FALSE) }

        # Observed and replicated
        ppc.data <- data.frame(obs = gh5[["bayesian_data/predictive/observed"]][, ],
                               rep = gh5[["bayesian_data/predictive/replicated"]][, ])

        # Difference between replicated and observed chi-square fit statistic
        ppc.data$diff <- ppc.data$obs - ppc.data$rep

        #### Posterior Predictive Check Scatterplot

        # 'xlab' argument
        if (isTRUE(is.null(xlab))) { xlab.pcc.scatter <- "Observed" } else { xlab.pcc.scatter <- xlab }

        # 'ylab' argument
        if (isTRUE(is.null(ylab))) { ylab.pcc.scatter <- "Replicated" } else { ylab.pcc.scatter <- ylab }

        # Axes limits
        limits.max <- 5L*ceiling(max(unlist(ppc.data[, c("obs", "rep")])) / 5L)

        # 'xbreaks' argument
        if (isTRUE(inherits(xbreaks, "waiver"))) { xbreaks.pcc.scatter <- seq(0L, limits.max, by = 5L) } else { xbreaks.pcc.scatter <- xbreaks }

        # 'ybreaks' argument
        if (isTRUE(inherits(ybreaks, "waiver"))) { ybreaks.pcc.scatter <- seq(0L, limits.max, by = 5L) } else { ybreaks.pcc.scatter <- ybreaks }

        # 'xlim' argument
        if (isTRUE(is.null(xlim))) { xlim.pcc.scatter <- c(0L, limits.max) } else { xlim.pcc.scatter <- xlim }

        # 'ylim' argument
        if (isTRUE(is.null(ylim))) { ylim.pcc.scatter <- c(0L, limits.max) } else { ylim.pcc.scatter <- ylim }

        # 'xexpand' argument
        if (isTRUE(inherits(xexpand, "waiver"))) { xexpand.ppc.scatter <- c(0.02, 0L) } else { xexpand.ppc.scatter <- xexpand }

        # 'yexpand' argument
        if (isTRUE(inherits(yexpand, "waiver"))) { yexpand.ppc.scatter <- c(0.02, 0L) } else { yexpand.ppc.scatter <- yexpand }

        # 'plot.margin' argument
        if (isTRUE(is.null(plot.margin))) { plot.margin.ppc.scatter <- c(2L, 15L, 4L, 4L) } else { plot.margin.ppc.scatter <- plot.margin }

        plot.ppc.scatter <- suppressMessages(ggplot2::ggplot(ppc.data, ggplot2::aes(x = obs, y = rep)) +
                              ggplot2::geom_point(shape = shape, fill = fill) +
                              ggplot2::geom_abline(slope = 1L) +
                              ggplot2::scale_x_continuous(name = xlab.pcc.scatter, limits = xlim.pcc.scatter, breaks = xbreaks.pcc.scatter, expand = xexpand.ppc.scatter) +
                              ggplot2::scale_y_continuous(name = ylab.pcc.scatter, limits = ylim.pcc.scatter, breaks = ybreaks.pcc.scatter, expand = yexpand.ppc.scatter) +
                              ggplot2::theme_bw() +
                              ggplot2::theme(plot.margin = ggplot2::margin(t = plot.margin.ppc.scatter[1L], r = plot.margin.ppc.scatter[2L], b = plot.margin.ppc.scatter[3L], l = plot.margin.ppc.scatter[4L])))

        #### Posterior Predictive Check Histogram

        # 'xlab' argument
        if (isTRUE(is.null(xlab))) { xlab.pcc.hist <- "Observed - Replicated" } else { xlab.pcc.hist <- xlab }

        # 'ylab' argument
        if (isTRUE(is.null(ylab))) { ylab.pcc.hist <- "Count" } else { ylab.pcc.hist <- ylab }

        # 'xbreaks' argument
        xbreaks.ppc.hist <- xbreaks

        # 'ybreaks' argument
        ybreaks.ppc.hist <- ybreaks

        # 'xlim' argument
        xlim.ppc.hist <- xlim

        # 'ylim' argument
        ylim.ppc.hist <- ylim

        # 'xexpand' argument
        if (isTRUE(inherits(xexpand, "waiver"))) { xexpand.ppc.hist <- c(0.02, 0L) } else { xexpand.ppc.scatter <- xexpand }

        # 'yexpand' argument
        if (isTRUE(inherits(yexpand, "waiver"))) { yexpand.ppc.hist <- ggplot2::expansion(mult = c(0L, 0.05))  } else { yexpand.ppc.scatter <- yexpand }

        # 'plot.margin' argument
        if (isTRUE(is.null(plot.margin))) { plot.margin.ppc.hist <- c(2L, 15L, 4L, 4L) } else { plot.margin.ppc.hist <- plot.margin }

        # Plot
        plot.ppc.hist <- suppressMessages(ggplot2::ggplot(ppc.data, ggplot2::aes(x = diff)) +
                           ggplot2::geom_histogram(binwidth = binwidth, bins = bins, color = "black", alpha = alpha, fill = fill) +
                           ggplot2::geom_vline(xintercept = mean(ppc.data$diff), color = point.col[1L]) +
                           ggplot2::geom_vline(xintercept = quantile(ppc.data$diff, probs = (1L - conf.level) / 2L), linetype = linetype, color = point.col[1L]) +
                           ggplot2::geom_vline(xintercept = quantile(ppc.data$diff, probs = 1L - (1L - conf.level) / 2L), linetype = linetype, color = point.col[1L]) +
                           ggplot2::scale_x_continuous(name = xlab.pcc.hist, limits = xlim.ppc.hist, breaks = xbreaks.ppc.hist, expand = xexpand.ppc.hist) +
                           ggplot2::scale_y_continuous(name = ylab.pcc.hist, limits = ylim.ppc.hist, breaks = ybreaks.ppc.hist, expand = yexpand.ppc.hist) +
                           ggplot2::theme_bw() +
                           ggplot2::theme(plot.margin = ggplot2::margin(t = plot.margin.ppc.hist[1L], r = plot.margin.ppc.hist[2L], b = plot.margin.ppc.hist[3L], l = plot.margin.ppc.hist[4L])))

        # Combine plots
        plot.ppc <- patchwork::wrap_plots(plot.ppc.scatter, plot.ppc.hist)

      } else {

        plot.ppc <- ppc.data <- NULL

      }

    } else {

      plot.trace <- plot.post <- plot.auto <- plot.ppc <- post <- post.wide <- post.long <- auto <- auto.wide <- auto.long <- ppc.data <- loop <- NULL

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## "loop_data" section ####

    if (isTRUE("loop_data" %in% names(gh5))) {

      #...................
      ### No "bayesian_data" section ####

      if (isTRUE(!"bayesian_data" %in% names(gh5))) { plot <- "loop" }

      #...................
      ### Extract Data ####

      # Multiple additional parameters
      if (isTRUE(nrow(gh5[["loop_data/estimates"]][, ]) != 1L)) { stop("The function cannot plot more than one variable at once.", call. = FALSE) }

      # Parameter Estimates
      plotdat.loop <- data.frame(estimate = gh5[["loop_data/estimates"]][, ],
                                 xval = gh5[["loop_data/xvalues"]][],
                                 low = gh5[["loop_data/lowerci"]][, ],
                                 upp = gh5[["loop_data/upperci"]][, ])

      # Statistical significance
      plotdat.loop$sig <- ifelse(plotdat.loop$estimate > 0L, ifelse(plotdat.loop$low > 0L, "p < .05", "n.s."), ifelse(plotdat.loop$upp < 0L, "p < .05", "n.s."))

      # Sections of statistical significance
      a <- 1L
      b <- 0L
      plotdat.loop$group <- NULL
      for (i in seq_along(plotdat.loop$sig)) {

        if (isTRUE(plotdat.loop$sig[i] == "n.s.")) {

          plotdat.loop$group[i] <- a

        } else {

          plotdat.loop$group[i] <- b
          a <- a + 1L

        }

      }

      # Change in statistical significance
      plotdat.loop$change <- c(0L, ifelse(diff(plotdat.loop$group) == 0L, 0L, 1L))

      # 'xlab' argument
      if (isTRUE(is.null(xlab))) { xlab.loop <- misty::chr.trim(hdf5r::h5attributes(gh5[["loop_data"]])$loop_variable) } else { xlab.loop <- xlab }

      # 'ylab' argument
      if (isTRUE(is.null(ylab))) { ylab.loop <- misty::chr.trim(hdf5r::h5attributes(gh5[["loop_data"]])$labels) } else { ylab.loop <- ylab }

      # 'xbreaks' argument
      xbreaks.loop <- xbreaks

      # 'ybreaks' argument
      ybreaks.loop <- ybreaks

      # 'xlim' argument
      xlim.loop <- xlim

      # 'ylim' argument
      ylim.loop <- ylim

      # 'xexpand' argument
      if (isTRUE(inherits(xexpand, "waiver"))) { xexpand.loop <- c(0.02, 0L) } else { xexpand.loop <- xexpand }

      # 'yexpand' argument
      if (isTRUE(inherits(yexpand, "waiver"))) { yexpand.loop <- c(0.02, 0L) } else { yexpand.loop <- yexpand }

      # 'plot.margin' argument
      if (isTRUE(is.null(plot.margin))) { if (isTRUE(area)) { plot.margin.loop <- c(4L, 15L, -6L, 4L) } else { plot.margin.loop <- c(4L, 15L, 2L, 4L) } } else { plot.margin.loop <- plot.margin }

      # 'legend.box.margin' argument
      if (isTRUE(is.null(legend.box.margin))) { legend.box.margin.loop <- c(-10L, 6L, 6L, 6L) } else { legend.box.margin.loop <- legend.box.margin }

      # Plot
      plot.loop <- suppressMessages(ggplot2::ggplot(plotdat.loop, ggplot2::aes(x = xval, y = estimate)) +
                     ggplot2::geom_line(linewidth = linewidth, show.legend = FALSE) +
                     ggplot2::geom_line(ggplot2::aes(xval, low)) +
                     ggplot2::geom_line(ggplot2::aes(xval, upp)) +
                     ggplot2::scale_x_continuous(name = xlab.loop, limits = xlim.loop, breaks = xbreaks.loop, expand = yexpand.loop) +
                     ggplot2::scale_y_continuous(name = ylab.loop, limits = ylim.loop, breaks = ybreaks.loop, expand = yexpand.loop) +
                     ggplot2::scale_fill_manual(name = "Statistical Significance", values = hcl.colors(n = 2L, palette = palette)) +
                     ggplot2::theme_bw() +
                     ggplot2::theme(plot.margin = ggplot2::margin(t = plot.margin.loop[1L], r = plot.margin.loop[2L], b = plot.margin.loop[3L], l = plot.margin.loop[4L]),
                                    legend.position = "bottom",
                                    legend.title = ggplot2::element_text(size = legend.title.size),
                                    legend.text = ggplot2::element_text(size = legend.text.size),
                                    legend.box.margin = ggplot2::margin(t = legend.box.margin.loop[1L], r = legend.box.margin.loop[2L], b = legend.box.margin.loop[3L], l = legend.box.margin.loop[4L]),
                                    legend.background = ggplot2::element_rect(fill = "transparent")))

      # Area and vertical lines
      if (isTRUE(isTRUE(area))) {

        for (i in unique(plotdat.loop$group)) { plot.loop <- suppressMessages(plot.loop +
                                                                                ggplot2::geom_ribbon(data = plotdat.loop[plotdat.loop$group == i, ], ggplot2::aes(ymin = low, ymax = upp, fill = sig), alpha = alpha)) }

        plot.loop <- suppressMessages(plot.loop +
                                        ggplot2::geom_vline(data = plotdat.loop[plotdat.loop$change == 1L, ], ggplot2::aes(xintercept = xval, color = sig), linewidth = linewidth, linetype = linetype, show.legend = FALSE) +
                                        ggplot2::geom_line(linewidth = linewidth, show.legend = FALSE) +
                                        ggplot2::geom_line(ggplot2::aes(xval, low)) +
                                        ggplot2::geom_line(ggplot2::aes(xval, upp)))

      }

    } else {

      plot.loop <- plotdat.loop <- NULL

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Close Mplus GH5 File ####

    hdf5r::h5close(gh5)

    #_____________________________________________________________________________
    #
    # Return Object --------------------------------------------------------------

    object <- list(call = match.call(),
                   type = "mplus",
                   x = gh5,
                   args = list(plot = plot, param = param, std = std, burnin = burnin,
                               point = point, ci = ci, chain  = chain, conf.level = conf.level,
                               hist = hist, density = density, area = area,
                               alpha = alpha, fill = fill, nrow = nrow, ncol = ncol,
                               scales = scales, xlab = xlab, ylab = ylab,
                               xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks,
                               xexpand = xexpand, yexpand = yexpand,
                               palette = palette, binwidth = binwidth, bins = bins,
                               density.col = density.col, shape = shape, point.col = point.col,
                               linewidth = linewidth, linetype = linetype, line.col = line.col,
                               bar.col = bar.col, bar.width = bar.width,
                               plot.margin = plot.margin, legend.title.size = legend.title.size,
                               legend.text.size = legend.text.size, legend.box.margin = legend.box.margin,
                               saveplot = saveplot, file = file, file.plot = file.plot,
                               width = width, height = height, units = units, dpi = dpi,
                               check = check),
                   data = list(post = list(parameters = post, wide = post.wide, long = post.long),
                               auto = list(autocor = auto, wide = auto.wide, long = auto.long),
                               ppc = ppc.data,
                               loop = plotdat.loop),
                   plot = list(trace = plot.trace, post = plot.post, auto = plot.auto, ppc = plot.ppc, loop = plot.loop))

    class(object) <- "misty.object"

  #----------------------------------------
  # Mplus Plots in misty object
  } else {

    x <- object

  }

  #_____________________________________________________________________________
  #
  # Plot -----------------------------------------------------------------------

  if (isTRUE(all(plot != "none"))) {

    switch(plot,
           # Trace plots
           trace = {

             if (isTRUE(is.null(object$plot$trace))) {

               stop("Trace plots not available in the Mplus GH5 file specified in the argument 'x'.", call. = FALSE)

             } else {

               suppressWarnings(suppressMessages(plot(object$plot$trace)))

             }

           },
           # Posterior distribution plots
           post = {

             if (isTRUE(is.null(object$plot$post))) {

               stop("Posterior parameter distribution plots are not available in the Mplus GH5 file specified in the argument 'x'.", call. = FALSE)

             } else {

               suppressWarnings(suppressMessages(plot(object$plot$post)))

             }

           },
           # Autocorrelation plots
           auto = {

             if (isTRUE(is.null(object$plot$auto))) {

               stop("Autocorrelation plots are not available in the Mplus GH5 file specified in the argument 'x'.", call. = FALSE)

             } else {

               suppressWarnings(suppressMessages(plot(object$plot$auto)))

             }

           },
           # Posterior predictive check plots
           ppc = {

             if (isTRUE(is.null(object$plot$ppc))) {

               stop("Posterior predictive check plots are not available in the Mplus GH5 file specified in the argument 'x'.", call. = FALSE)

             } else {

               suppressWarnings(suppressMessages(plot(object$plot$ppc)))

             }

           },
           # Loop plot
           loop = {

             if (isTRUE(is.null(object$plot$loop))) {

               stop("Loop plot is not available in the Mplus GH5 file specified in the argument 'x'.", call. = FALSE)

             } else {

               suppressWarnings(suppressMessages(plot(object$plot$loop)))

             }

           })

  }

  #_____________________________________________________________________________
  #
  # Save ggplot ----------------------------------------------------------------

  if (isTRUE(all(saveplot != "none"))) {

    # File extension
    file.ext <- paste0(".", rev(unlist(strsplit(file, "\\.")))[1L])

    # Trace plot
    if (isTRUE("trace" %in% saveplot)) {

      if (isTRUE(is.null(object$plot$trace))) {

        stop("Trace plots are not available in the Mplus GH5 file specified in the argument 'x'.", call. = FALSE)

      } else {

        suppressWarnings(suppressMessages(ggplot2::ggsave(filename = sub(file.ext, paste0(file.plot[1L], file.ext), file), plot = object$plot$trace, width = width, height = height, units = units, dpi = dpi)))

      }

    }

    # Posterior distribution plot
    if (isTRUE("post" %in% saveplot)) {

      if (isTRUE(is.null(object$plot$post))) {

        stop("Posterior parameter distribution plots are not available in the Mplus GH5 file specified in the argument 'x'.", call. = FALSE)

      } else {

        suppressWarnings(suppressMessages(ggplot2::ggsave(filename = sub(file.ext, paste0(file.plot[2L], file.ext), file), plot = object$plot$post, width = width, height = height, units = units, dpi = dpi)))

      }

    }

    # Autocorrelation plot
    if (isTRUE("auto" %in% saveplot)) {

      if (isTRUE(is.null(object$plot$post))) {

        stop("Autocorrelation plots are not available in the Mplus GH5 file specified in the argument 'x'.", call. = FALSE)

      } else {

        suppressWarnings(suppressMessages(ggplot2::ggsave(filename = sub(file.ext, paste0(file.plot[3L], file.ext), file), plot = object$plot$auto, width = width, height = height, units = units, dpi = dpi)))

      }

    }

    # Posterior Predictive check plot
    if (isTRUE("ppc" %in% saveplot)) {

      if (isTRUE(is.null(object$plot$ppc))) {

        stop("Posterior predictive check plots are not available in the Mplus GH5 file specified in the argument 'x'.", call. = FALSE)

      } else {

        suppressWarnings(suppressMessages(ggplot2::ggsave(filename = sub(file.ext, paste0(file.plot[4L], file.ext), file), plot = object$plot$ppc, width = width, height = height, units = units, dpi = dpi)))

      }

    }

    # Loop plot
    if (isTRUE("loop" %in% saveplot)) {

      if (isTRUE(is.null(object$plot$loop))) {

        stop("Loop plot is not available in the Mplus GH5 file specified in the argument 'x'.", call. = FALSE)

      } else {

        suppressWarnings(suppressMessages(ggplot2::ggsave(filename = sub(file.ext, paste0(file.plot[5L], file.ext), file), plot = object$plot$loop, width = width, height = height, units = units, dpi = dpi)))

      }

    }

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(invisible(object))

}
