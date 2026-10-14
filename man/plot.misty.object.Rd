\name{plot.misty.object}
\alias{plot.misty.object}
\title{Plots misty.object object}
\usage{
\method{plot}{misty.object}(x, plot = NULL, geom = NULL, color = NULL, type = NULL,
     line.type = NULL, line.width = NULL, line.col = NULL,
     ci.col = NULL, ci.width = NULL, ci.type = NULL,
     errorbar.width = NULL, errorbar.dodge.width = NULL,
     smooth.fill = NULL, smooth.alpha = NULL, smooth.type = NULL,
     smooth.width = NULL, smooth.col = NULL,
     point.size = NULL, point.width = NULL, point.type = NULL,
     point.shape = NULL, point.col = NULL, point.fill = NULL,
     jitter.size = NULL, jitter.width = NULL,
     jitter.height = NULL, jitter.alpha = NULL,
     ribbon.fill = NULL, ribbon.col = NULL, ribbon.alpha = NULL,
     violin.alpha = NULL, violin.trim = NULL,
     density.col = NULL, density.width = NULL,
     density.type = NULL, density.alpha = NULL,
     hist.binwidth = NULL, hist.bins = NULL, hist.col = NULL,
     hist.fill = NULL, hist.alpha = NULL,
     box.width = NULL, box.alpha = NULL, tile.col = NULL, tile.alpha = NULL,
     function.type = NULL, function.width = NULL, function.col = NULL,
     abline.col = NULL, abline.width = NULL,
     hline.intercept = NULL, hline.type = NULL, hline.width = NULL, hline.col = NULL,
     vline.intercept = NULL, vline.type = NULL, vline.width = NULL, vline.col = NULL,
     text.size = NULL, text.col = NULL, text.font = NULL,
     gray = NULL, start = NULL, end = NULL,
     xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL,
     xbreaks = NULL, ybreaks = NULL,
     axis.title.size = NULL, axis.text.size = NULL, strip.text.size = NULL,
     scale.fill.col = NULL, scale.fill.low = NULL, scale.fill.high = NULL,
     scale.fill.mid = NULL, title = NULL, subtitle = NULL, group.col = NULL,
     plot.margin = NULL, legend.title = NULL, legend.position = NULL,
     legend.box.margin = NULL, legend.key.size = NULL, legend.text.size = NULL,
     facet.ncol = NULL, facet.nrow = NULL, facet.scales = NULL,
     conf.level = NULL, adjust = NULL,
     filename = NULL, width = NULL, height = NULL, units = NULL, dpi = NULL,
     check = TRUE, ...)
}
\arguments{

\item{x}{\code{misty.object} object.}

\item{plot}{see 'Details'}

\item{geom}{see 'Details'}

\item{color}{see 'Details'}

\item{type}{see 'Details'}

\item{line.type}{see 'Details'}

\item{line.width}{see 'Details'}

\item{line.col}{see 'Details'}

\item{ci.col}{see 'Details'}

\item{ci.width}{see 'Details'}

\item{ci.type}{see 'Details'}

\item{errorbar.width}{see 'Details'}

\item{errorbar.dodge.width}{see 'Details'}

\item{smooth.fill}{see 'Details'}

\item{smooth.alpha}{see 'Details'}

\item{smooth.type}{see 'Details'}

\item{smooth.width}{see 'Details'}

\item{smooth.col}{see 'Details'}

\item{point.size}{see 'Details'}

\item{point.width}{see 'Details'}

\item{point.type}{see 'Details'}

\item{point.shape}{see 'Details'}

\item{point.col}{see 'Details'}

\item{point.fill}{see 'Details'}

\item{jitter.size}{see 'Details'}

\item{jitter.width}{see 'Details'}

\item{jitter.height}{see 'Details'}

\item{jitter.alpha}{see 'Details'}

\item{ribbon.fill}{see 'Details'}

\item{ribbon.col}{see 'Details'}

\item{ribbon.alpha}{see 'Details'}

\item{violin.alpha}{see 'Details'}

\item{violin.trim}{see 'Details'}

\item{density.col}{see 'Details'}

\item{density.width}{see 'Details'}

\item{density.type}{see 'Details'}

\item{density.alpha}{see 'Details'}

\item{hist.binwidth}{see 'Details'}

\item{hist.bins}{see 'Details'}

\item{hist.col}{see 'Details'}

\item{hist.fill}{see 'Details'}

\item{hist.alpha}{see 'Details'}

\item{box.width}{see 'Details'}

\item{box.alpha}{see 'Details'}

\item{tile.col}{see 'Details'}

\item{tile.alpha}{see 'Details'}

\item{function.type}{see 'Details'}

\item{function.width}{see 'Details'}

\item{function.col}{see 'Details'}

\item{abline.col}{see 'Details'}

\item{abline.width}{see 'Details'}

\item{hline.intercept}{see 'Details'}

\item{hline.type}{see 'Details'}

\item{hline.width}{see 'Details'}

\item{hline.col}{see 'Details'}

\item{vline.intercept}{see 'Details'}

\item{vline.type}{see 'Details'}

\item{vline.width}{see 'Details'}

\item{vline.col}{see 'Details'}

\item{text.size}{see 'Details'}

\item{text.col}{see 'Details'}

\item{text.font}{see 'Details'}

\item{gray}{see 'Details'}

\item{start}{see 'Details'}

\item{end}{see 'Details'}

\item{xlab}{see 'Details'}

\item{ylab}{see 'Details'}

\item{xlim}{see 'Details'}

\item{ylim}{see 'Details'}

\item{xbreaks}{see 'Details'}

\item{ybreaks}{see 'Details'}

\item{axis.title.size}{see 'Details'}

\item{axis.text.size}{see 'Details'}

\item{strip.text.size}{see 'Details'}

\item{scale.fill.col}{see 'Details'}

\item{scale.fill.low}{see 'Details'}

\item{scale.fill.high}{see 'Details'}

\item{scale.fill.mid}{see 'Details'}

\item{title}{see 'Details'}

\item{subtitle}{see 'Details'}

\item{group.col}{see 'Details'}

\item{plot.margin}{see 'Details'}

\item{legend.title}{see 'Details'}

\item{legend.position}{see 'Details'}

\item{legend.box.margin}{see 'Details'}

\item{legend.key.size}{see 'Details'}

\item{legend.text.size}{see 'Details'}

\item{facet.ncol}{see 'Details'}

\item{facet.nrow}{see 'Details'}

\item{facet.scales}{see 'Details'}

\item{conf.level}{see 'Details'}

\item{adjust}{see 'Details'}

\item{filename}{a character string indicating the \code{filename}
argument including the file extension in the \code{ggsave}
function. Note that one of \code{".eps"}, \code{".ps"},
\code{".tex"}, \code{".pdf"} (default),
\code{".jpeg"}, \code{".tiff"}, \code{".png"},
\code{".bmp"}, \code{".svg"} or \code{".wmf"} needs
to be specified as file extension in the \code{file}
argument.}

\item{width}{a numeric value indicating the \code{width} argument
for the \code{ggsave} function.}

\item{height}{a numeric value indicating the \code{height} argument
(default is the size of the current graphics device)
for the \code{ggsave} function.}

\item{units}{a character string indicating the \code{units} argument
in the \code{ggsave} function. Note that one of
\code{"in"}, \code{"cm"}, \code{"mm"}, or \code{"px"}
needs to be specified.}

\item{dpi}{a numeric value indicating the \code{dpi} argument
for the \code{ggsave} function.}

\item{check}{logical: if \code{TRUE} (default), argument specification
is checked.}

\item{...}{further arguments passed to or from other methods.}
}
\description{
This function plots an \code{misty.object} object.
}
\details{
This function provides plotting arguments depending on the type of the output
object specified for the argument \code{x}. Note that the arguments \code{filename},
\code{width}, \code{height}, \code{units}, and \code{dpi} are available for
all types of the output object.
\describe{
\item{\strong{Functions \code{aov.b} and \code{aov.w}}}{The \code{plot} function
for the misty object of type \code{"aov.b"}, \code{"aov.w"}, and \code{test.welch}
has following plotting arguments:
\itemize{
   \item{\code{geom}}: a character vector indicating which plot elements to draw,
   i.e., \code{"all"} for drawing all plot elements, \code{"bar"} for drawing
   bars representing means for each groups, \code{"point"} for drawing points
   representing means for each group, \code{"line"} for drawing a line connecting
   means of each group and lines connecting data points when requesting \code{"jitter"},
   \code{"ci"} for drawing error bars representing confidence intervals, and
   \code{"jitter"} for drawing jittered data points.
   \item{\code{conf.level}}: a numeric value between 0 and 1 (default: \code{0.95})
   indicating the confidence level of the interval.
   \item{\code{adjust}}: logical: if \code{TRUE} (default), difference-adjustment
   for the confidence intervals is applied.
   \item{\code{point.size}}: a numeric value (default: \code{3}) indicating the
   \code{size} argument in the \code{geom_point} function.
   \item{\code{line.type}}: a numeric value (default: \code{"solid"}) indicating
   the \code{linetype} argument for the \code{geom_line} function.
   Note that this argument is only available for the \code{"aov.w"} function.
   \item{\code{line.width}}: a numeric value (default: \code{0.5}) indicating
   the \code{linewidth} argument for the \code{geom_line} function.
   Note that this argument is only available for the \code{"aov.w"} function.
   \item{\code{errorbar.width}}: a numeric value (default: \code{0.1}) indicating
   the \code{width} argument for the \code{geom_errorbar} function.
   \item{\code{jitter.size}}: a numeric value (default: \code{1.25}) indicating
   the \code{size} argument for the \code{geom_jitter} function.
   \item{\code{jitter.width}}: a numeric value (default: \code{0.05}) indicating
   the \code{width} argument for the \code{geom_jitter} function.
   \item{\code{jitter.height}}: a numeric value (default: \code{0}) indicating
   the \code{height} argument for the \code{geom_jitter} function
   \item{\code{jitter.alpha}}: a numeric value between 0 and 1 (default: \code{0.1})
   the \code{alpha} argument for the \code{geom_jitter} function
   \item{\code{xlab}}: a character string (default: \code{NULL}) indicating
   the \code{name} argument in the \code{scale_x_discrete} function.
   \item{\code{ylab}}: a character string (default: \code{"y"}) indicating
   the \code{name} argument in the \code{scale_y_continuous} function.
   \item{\code{ylim}}: a numeric vector with two elements (default: \code{NULL})
   indicating the \code{limits} argument in the \code{scale_y_continuous} function.
   \item{\code{ybreaks}}: a numeric vector (default: \code{waiver()}) indicating
   the \code{breaks} argument in the \code{scale_y_continuous} function.
   \item{\code{title}}: a character string (default: \code{""}) indicating the
   \code{title} argument in the \code{labs} function.
   \item{\code{subtitle}}: a character string indicating the \code{subtitle}
   argument in the \code{labs} function. The default is \code{"Two-Sided Confidence Interval"}
   when \code{adjust = FALSE} or \code{"Two-Sided Difference-Adjusted Confidence Interval"}
   for the \code{aov.b} function and
   \code{"Two-Sided Difference-Adjusted Cousineau-Morey Confidence Interval Confidence Interval"}
   for the \code{aov.w} function when \code{adjust = TRUE}.
}
}
\item{\strong{Function \code{boot.bs}}}{The ggplot2 arguments provided for
  the function \code{boot.bs} are described in the help page of the
  \code{\link{boot.bs}} function.
}
\item{\strong{Function \code{check.outlier}}}{The \code{plot} function for the
misty object of type \code{"check.outlier"} has following plotting arguments:
\itemize{
   \item{\code{type}}:  character vector indicating which statistical measures to
   include in the plot, i.e., \code{"mahal"} for the Mahalanobis distance,
   \code{"hat"} for the hat value, \code{"rstand"} for the standardized
   leverage-corrected residual, \code{"rstud"} for the studentized leverage-corrected
   residual, \code{"cook"} for the Cook's distance, and \code{"dfb"} for the
   DFBETAS.
   \item{\code{geom}}: a character vector indicating which plot elements to draw,
   i.e., \code{"none"} for not drawing any labels, \code{"text"} (default)
   for drawing labels above or below each observation.
   \item{\code{point.size}}: a numeric value (default: \code{2}) indicating
   the \code{size} argument in the \code{geom_point} function.
   \item{\code{point.shape}}: a numeric value (default: \code{21}) indicating
   the \code{shape} argument in the \code{geom_point} function.
   \item{\code{point.fill}}: a character string  (default: \code{"#648FFF"})
   indicating the \code{fill} argument in the \code{geom_point} function.
   \item{\code{point.col}}: a character string  (default: \code{"black"})
   indicating the \code{color} argument in the \code{geom_point} function.
   \item{\code{line.type}}: an integer value or character string (default:
   \code{"solid"}) indicating the \code{linetype} argument in the \code{geom_linerange}
   function.
   \item{\code{line.width}}: a numeric value (default: \code{0.5}) indicating
   the \code{linewidth} argument in the \code{geom_linerange} function.
   \item{\code{line.col}}: a character string (default: \code{"#648FFF"}) indicating
   the \code{color} argument in the \code{geom_linerange} function.
   \item{\code{hline.type}}: an integer value or character string (default:
   \code{"dashed"}) indicating the \code{linetype} argument in the \code{geom_hline}
   function.
   \item{\code{hline.width}}: a numeric value (default: \code{0.5}) indicating
   the \code{linewidth} argument in the \code{geom_hline} function.
   \item{\code{hline.col}}: a character string (default: \code{"#FE6100"}) indicating
   the \code{color} argument in the \code{geom_hline} function.
   the \code{color} argument in the \code{geom_hline} function.
   \item{\code{text.size}}: a numeric value (default: \code{3}) indicating the
   \code{size} argument in the \code{geom_text} function.
   \item{\code{text.col}}: a character string (default: \code{"black"}) indicating
   the \code{color} argument in the \code{geom_text} function.
   \item{\code{ylim}}: a numeric vector with two elements indicating the \code{limits}
   argument in the \code{scale_y_continuous} function.
   \item{\code{ybreaks}}: a numeric vector (default: \code{waiver()}) indicating
   the \code{breaks} argument in the \code{scale_y_continuous} function.
   \item{\code{axis.title.size}}: a numeric value (default: \code{11}) indicating
   the \code{size} argument in the \code{element_text} function, i.e.
   \code{theme(axis.title = element_text(size = axis.text.size))}
   \item{\code{axis.text.size}}: a numeric value (default: 10) indicating the
   \code{size} argument in the \code{element_text} function, i.e.,
   \code{theme(axis.text = element_text(size = axis.text.size))}
}
}
\item{\strong{Function \code{check.resid}}}{The \code{plot} function for the
misty object of type \code{"check.resid"} have the same arguments as the
\code{test.linear} function when \code{type = "linear"}, the \code{test.homo}
function when \code{type = "homo"}, and the \code{test.normal} function when
\code{type = "test.norm"}. In addition, the \code{plot} function for the
misty object of type \code{"check.resid"} has following plotting argument:
\itemize{
   \item{\code{geom}}: a character vector indicating which plot elements to
   draw, i.e., \code{"all"} for drawing all plot elements, \code{"lm"} for
   drawing the regression line, \code{"loess"} for drawing the LOESS smoothing
   curve, \code{"ci.lm"} for drawing confidence bands around the regression
   line, \code{"ci.loess"} for drawing confidence bands around the LOESS smoothing
   curve, \code{"hline"} or drawing a horizontal line at \code{0}, \code{"qq"}
   for plotting Q-Q plots, and \code{"hist"} for plotting histograms. Note that
   the arguments \code{"lm"}, \code{"loess"}, \code{"ci.lm"}, and \code{"ci.less"}
   are applicable when \code{type = "linear"}, the arguments \code{"hline"},
   \code{"loess"}, and \code{"ci.loess"} are applicable when \code{type = "homo"},
   and the arguments \code{"lqq"} and \code{"hist"} are applicable when
   \code{type = "norm"}.
}
}
\item{\strong{Functions \code{ci.*}}}{The \code{plot} function for the misty
object of type \code{"ci.cor"}, \code{"ci.mean"}, \code{"ci.median"},
\code{"ci.prop"}, \code{"ci.var"}, and \code{"ci.sd"} has following plotting
arguments:
\itemize{
   \item{\code{plot}}: a character string indicating the type of the plot
   to display, i.e., \code{"ci"} (default) for displaying confidence intervals
   or \code{"boot"} for displaying bootstrap samples with histograms and density
   curves when the argument.
   \item{\code{geom}}: a character vector indicating which plot elements to draw,
   i.e., \code{"all"} for drawing all plot elements,\code{"hist"} for drawing histograms
   when plotting bootstrap samples (\code{plot = "boot"}), \code{"density"}
   for drawing density curves when plotting bootstrap samples (\code{plot = "boot"}),
   \code{"point"} for drawing vertical lines representing the point estimate
   of the correlation coefficients when plotting bootstrap samples
   (\code{plot = "boot"}), \code{"ci"} for drawing vertical lines representing
   the bootstrap confidence intervals of the correlation when plotting bootstrap
   samples (\code{plot = "boot"}), and \code{"line"} for drawing a horizontal
   line when specifying \code{plot = "ci"} or a vertical line when specifying
   \code{plot = "boot"}.
   \item{\code{point.size}}: a numeric value (default: \code{2.5}) indicating
   the \code{size} argument in the \code{geom_point} function.
   \item{\code{point.shape}}: a numeric value between 0 and 25 (default: \code{19})
   indicating the \code{shape} argument in the \code{geom_point} function.
   \item{\code{errorbar.width}}: a numeric value (default: \code{0.3}) indicating
   the \code{width} argument in the \code{geom_errorbar} function.
   \item{\code{errorbar.dodge.width}}: a numeric value (default: \code{0.5}) indicating
   the \code{width} argument in the \code{geom_errorbar} function.
   \item{\code{hist.bins}}: a numeric value or a function (default: \code{NULL})
   indicating the \code{bins} argument in the \code{geom_histogram} function.
   \item{\code{hist.binwidth}}: a numeric value or a function (default: \code{NULL})
   indicating the \code{binwidth} argument in the \code{geom_histogram} function.
   \item{\code{hist.alpha}}: a numeric value between 0 and 1 (default: \code{0.4})
   indicating the \code{alpha} argument in the \code{geom_histogram}
   function.
   \item{\code{hist.fill}}: a character string (default: \code{"gray85"}) indicating
   the \code{fill} argument in the \code{geom_histogram} function.
   \item{\code{density.col}}: a character string (default: \code{"#0072B2"})
   indicating the \code{color} argument in the \code{geom_density} function.
   \item{\code{density.width}}: a numeric value (default: \code{0.5}) indicating
   the \code{linewidth} argument in the \code{geom_density} function.
   \item{\code{density.type}}: a numeric value or character string (default:
   \code{0.5}) indicating the \code{linetype} argument in the \code{geom_density}
   function.
   \item{\code{point.col}}: a character string (default: \code{"#CC79A7"}) indicating
   the \code{color} argument in the \code{geom_vline} function. Note that this
   argument applied only when no grouping variable was specified \code{group = NULL}.
   \item{\code{point.width}}: a numeric value (default: \code{0.6}) indicating
   the \code{linewdith} argument in the \code{geom_vline} function.
   \item{\code{point.type}}: a numeric value or character string (default:
   \code{"solid"}) indicating the \code{linetype} argument in the \code{geom_vline}
   function.
   \item{\code{ci.col}}: character string (default: \code{"black"}) indicating the
   \code{color} argument in the \code{geom_vline} function. Note that this argument
   applied only when no grouping variable was specified \code{group = NULL}.
   \item{\code{ci.width}}: a numeric value (default: \code{0.6}) indicating
   the \code{linewdith} argument in the \code{geom_vline} function.
   \item{\code{ci.type}}: a numeric value or character string (default:
   \code{"dashed"}) indicating the \code{linetype} argument in the \code{geom_vline}
   function.
   \item{\code{hline.intercept}}: a numeric value (default = \code{0}) indicating the
   \code{yintercept} argument in the \code{geom_hline} function.
   \item{\code{hline.type}}: a character string (default: \code{"solid"}) indicating
   the \code{linetype} argument in the \code{geom_hline} function.
   \item{\code{hline.col}}: a character string (default: \code{"gray65"}) indicating
   the \code{color} argument in the \code{geom_hline} function.
   \item{\code{vline.intercept}}: a numeric value (default = \code{0}) indicating the
   \code{xintercept} argument in the \code{geom_vline} function.
   \item{\code{vline.type}}: a character string (default: \code{"solid"}) indicating
   the \code{linetype} argument in the \code{geom_vline} function.
   \item{\code{vline.col}}: a character string (default: \code{"gray65"}) indicating
   the \code{color} argument in the \code{geom_vline} function.
   \item{\code{xlab}}: a character string indicating the \code{name} argument
   in the \code{scale_x_continuous} function. The default setting is \code{xlab = NULL}
   when \code{plot = "ci"} and    \code{xlab = "Correlation Coefficient"},
   \code{xlab = "Arithmetic Mean"}, \code{xlab = "Median"}, \code{xlab = "Proportion"},
   \code{xlab = "Variance"}, or \code{xlab = "Standard Deviation"}.
   \item{\code{ylab}}: a character string indicating the \code{name} argument
   in the \code{scale_y_continuous} function. The default setting is
   \code{ylab = "Correlation Coefficient"}, \code{ylab = "Arithmetic Mean"},
   \code{ylab = "Median"}, \code{ylab = "Proportion"}, \code{ylab = "Variance"},
   or \code{ylab = "Standard Deviation"} when \code{plot = "ci"} and
   \code{ylab = "Probability Density f(x)"} when \code{plot = "boot"}.
   \item{\code{xlim}}: a numeric vector with two elements indicating the
   \code{limits} argument in the \code{scale_x_continuous} function. The default
   setting is \code{xlim = NULL} when \code{plot = "ci"} and \code{xlim = c(-1, 1)}
   for the correlation coefficient and proportion or \code{xlim = NULL)} for
   the arithmetic mean, median, variance and standard deviation when \code{plot = "boot"}.
   \item{\code{ylim}}: a numeric vector with two elements indicating the
   \code{limits} argument in the \code{scale_y_continuous} function. The default
   setting is \code{ylim = c(-1, 1)} for the correlation coefficient and proportion
   and \code{ylim = NULL} for the arithmetic mean, median, variance and standard
   deviation when \code{plot = "ci"} and \code{xlim = NULL} when \code{plot = "boot"}.
   \item{\code{xbreaks}}: a numeric vector (default: \code{waiver()}) indicating
   the \code{breaks} argument in the \code{scale_x_continuous} function.
   \item{\code{ybreaks}}: a numeric vector (default: \code{waiver()}) indicating
   the \code{breaks} argument in the \code{scale_y_continuous} function.
   \item{\code{axis.title.size}}: a numeric value (default: \code{11}) indicating
   the \code{size} argument in the \code{element_text} function, i.e.
   \code{theme(axis.title = element_text(size = axis.text.size))}
   \item{\code{axis.text.size}}: a numeric value (default: 10) indicating the
   \code{size} argument in the \code{element_text} function,
   i.e. \code{theme(axis.text = element_text(size = axis.text.size))}.
   \item{\code{strip.text.size}}: a numeric value (default: \code{11}) indicating
   the \code{size} argument in the \code{element_text} function, i.e.
   \code{theme(strip.text = element_text(size = strip.text.size))}.
   \item{\code{title}}: a character string (default: \code{NULL}) indicating the
   \code{title} argument in the \code{labs} function.
   \item{\code{subtitle}}: a character string (default: \code{NULL}) indicating
   the \code{subtite} argument in the \code{labs} function.
   \item{\code{group.col}}: a character vector (default: \code{NULL}) indicating
   the \code{color} argument in the \code{scale_color_manual} and \code{scale_fill_manual}
   functions when specifying a grouping variable using the argument \code{group}.
   \item{\code{plot.margin}}: a numeric vector (default: \code{NA}) with four
   elements indicating the \code{plot.margin} argument in the \code{theme} function.
   The default setting is \code{c(5.5, 5.5, 5.5, 5.5)} but switches to
   \code{c(5.5, 5.5, -2.5, 5.5)} when specifying a grouping variable using
   the argument \code{group}.
   \item{\code{legend.title}}: a character string (default: \code{""}) indicating
   the \code{color} argument in the \code{labs} function when specifying a
   grouping variable using the argument \code{group}.
   \item{\code{legend.position}}: a character string (default: \code{"bottom"})
   indicating the \code{legend.position} in the \code{theme} argument when
   specifying a grouping variable using the argument \code{group}.
   \item{\code{legend.box.margin}}: a numeric vector (default: \code{c(-10, 0, 0, 0)})
   with four elements indicating the \code{legend.box.margin} argument in the
   \code{theme} function when specifying a grouping variable using the argument
   \code{group}.
   \item{\code{facet.ncol}}: a numeric value (default: \code{NULL}) indicating the
   \code{ncol} argument in the \code{facet_wrap} when specifying a split variable
   using the argument \code{split}.
   \item{\code{facet.nrow}}: a numeric value (default: \code{NULL}) indicating the
   \code{nrow} argument in the \code{facet_wrap} function when specifying a
   split variable using the argument \code{split}.
   \item{\code{facet.scales}}: a character string (default: \code{"free_y"})
   indicating the \code{scales} argument in the \code{facet_wrap} function,
   i.e. \code{"fixed"}, \code{"free_x"}, \code{"free_y"} (default) or \code{"free"}
   when specifying a split variable using the argument \code{split}.
}
}
\item{\strong{Functions \code{cor.matrix} and \code{multilevel.cor}}}{
The \code{plot} functions for the misty object of type \code{"cor.matrix"}
and \code{"multilevel.cor"} function have  following plotting arguments:
\itemize{
   \item{\code{tile.col}}: a character string (default: \code{"black"})
   indicating the \code{color} argument in the \code{geom_tile} function.
   \item{\code{scale.fill.low}}: a character string (default: \code{"#D41159"})
   indicating the \code{low} argument in the \code{scale_fill_gradient2} function.
   \item{\code{scale.fill.high}}: a character string (default: \code{"#1A85FF"})
   indicating the \code{high} argument in the \code{scale_fill_gradient2} function.
   \item{\code{scale.fill.mid}}: a character string (default: \code{"#1A85FF"})
   indicating the \code{mid} argument in the \code{scale_fill_gradient2} function.
   \item{\code{text.size}}: a numeric vector with two elements (default: \code{c(3.5, 3.5)})
   indicating the \code{size} argument in the \code{geom_text} function.
   \item{\code{text.col}}: a character vector with two elements (default: \code{c("black", "black")})
   indicating the \code{color} argument in the \code{geom_text} function.
   \item{\code{text.font}}: a character vector with two elements (default: \code{c("plain", "plain")})
   indicating the \code{fontface} argument in the \code{geom_text} function.
}
}
\item{\strong{Function \code{item.dfi}}}{The ggplot2 arguments provided for
  the function \code{item.dfi} are described in the help page of the
  \code{\link{item.dfi}} function.
}
\item{\strong{Functions \code{multilevel.r2} and \code{multilevel.r2.manual}}}{
The \code{plot} functions for the misty object of type \code{"multilevel.r2"}
and \code{"multilevel.r2.manual"} have following plotting arguments:
\itemize{
   \item{\code{scale.fill.col}}: a character vector, graphical parameter indicating
   the color of bars in the bar chart in the following order: Fixed slopes (Within),
   Fixed slopes (Between), Slope variation (Within), Intercept variation
   (Between), and Residual (Within). The default setting is
   \code{c("#D55E00", "#0072B2", "#CC79A7", "#009E73", "#E69F00")}.
   \item{\code{gray}}: logical: if \code{TRUE}, graphical parameter to draw
   the bar chart in gray scale.
   \item{\code{start}}: a numeric value between 0 and 1 (default: \code{0.15}),
   graphical parameter to specify the gray value at the low end of the palette.
   \item{\code{end}}: a numeric value between 0 and 1 (default: \code{0.85}),
   graphical parameter to specify the gray value at the high end of the palette.
}
}
\item{\strong{Function \code{na.pattern}}}{
The \code{plot} functions for the misty object of type \code{"na.pattern"}
function have following plotting arguments:
\itemize{
   \item{\code{geom}}: a character vector indicating which plot elements to
   draw, i.e., \code{"all"} for drawing all plot elements, \code{"square"} (default)
   for drawing squares that mimic the \code{md.pattern} function in the package
   \pkg{mice}, and \code{"rotate"}, to rotate variable name labels 90 degrees.
   \item{\code{tile.alpha}}: a numeric value between 0 and 1 (default: \code{0.6})
   for the \code{alpha} argument (default is \code{0.1}).
   \item{\code{scale.fill.col}}: a character string indicating the color for the
   \code{"fill"} argument. Note that the first color represents missing values
   and the second color represent observed values. The default setting is
   \code{c("#B61A51B3", "#006CC2B3")}.
   \item{\code{plot.margin}}: a numeric vector indicating the \code{plot.margin}
   argument for the \code{margin} function for the \code{plot.margin} argument
   within the \code{theme} function. The default setting is \code{c(4, 16, 0, 4)}.
   \item{\code{legend.box.margin}}: a numeric vector indicating the \code{legend.box.margin}
   argument for the \code{element_text} function for the \code{legend.text}
   argument within the \code{theme} function. The default setting is
   \code{c(-8, 6, 6, 6)}.
   \item{\code{legend.key.size}}: a numeric value (default: \code{unit(12, "pt")})
   indicating  the \code{legend.key} argument for the \code{theme} function.
}
}
\item{\strong{Function \code{test.homo}}}{The \code{plot} function for the
misty object of type \code{"test.homo"} has following plotting arguments:
\itemize{
   \item{\code{geom}}: a character vector indicating which plot elements to draw,
   i.e., \code{"all"} (default) for drawing all plot elements, \code{"none"}
   for drawing no horizontal or smoothing curve, \code{"hline"} for drawing a
   horizontal line at \code{0} using the function \code{ggplot2::geom_hline},
   \code{"loess"} for drawing the LOESS smoothing curve using the function
   \code{ggplot2::geom_smooth}, and \code{"ci.loess"} for drawing confidence bands
   around the LOESS smoothing curve (i.e., \code{se = TRUE}).
   \item{\code{hline.type}}: a numeric value (default: \code{"solid"})
   indicating the \code{linetype} argument in the \code{geom_hline} function.
   \item{\code{hline.width}}: a numeric value (default: \code{1})
   indicating the \code{linewidth} argument in the \code{geom_hline} function.
   \item{\code{hline.col}}: a character string (default: \code{"#FE6100"})
   indicating the \code{color} argument in the \code{geom_hline} function.
   \item{\code{point.size}}: a numeric value (default: \code{2}) indicating
   the \code{size} argument in the \code{geom_point} function.
   \item{\code{point.shape}}: a numeric value (default: \code{21}) indicating
   the \code{shape} argument in the \code{geom_point} function.
   \item{\code{point.fill}}: a character string  (default: \code{"#648FFF"})
   indicating the \code{fill} argument in the \code{geom_point} function.
   \item{\code{point.col}}: a character string  (default: \code{"black"})
   indicating the \code{color} argument in the \code{geom_point} function.
   \item{\code{smooth.fill}}: a character string (default: \code{"##56B4E9"})
   indicating the \code{fill} argument in the \code{geom_smooth} function.
   \item{\code{smooth.alpha}}: a numeric value between 0 and 1 (default: \code{0.15})
   indicating the \code{alpha} argument in the \code{geom_smooth} function.
   \item{\code{smooth.type}}: a numeric value (default: \code{"dashed"})
   indicating the \code{linetype} argument in the \code{geom_smooth} function.
   \item{\code{smooth.width}}: a numeric value (default: \code{1})
   indicating the \code{linewidth} argument in the \code{geom_smooth} function.
   \item{\code{smooth.col}}: a character string (default: \code{"#0057B8"})
   indicating the \code{color} argument in the \code{geom_smooth} function.
   \item{\code{xlim}}: a numeric vector with two elements indicating the \code{limits}
   argument in the \code{scale_x_continuous} function.
   \item{\code{xbreaks}}: a numeric vector (default: \code{waiver()}) indicating
   the \code{breaks} argument in the \code{scale_x_continuous} function.
   \item{\code{ylim}}: a numeric vector with two elements indicating the \code{limits}
   argument in the \code{scale_y_continuous} function.
   \item{\code{ybreaks}}: a numeric vector (default: \code{waiver()}) indicating
   the \code{breaks} argument in the \code{scale_y_continuous} function.
   \item{\code{axis.title.size}}: a numeric value (default: \code{11}) indicating
   the \code{size} argument in the \code{element_text} function, i.e.
   \code{theme(axis.title = element_text(size = axis.text.size))}
   \item{\code{axis.text.size}}: a numeric value (default: 10) indicating the
   \code{size} argument in the \code{element_text} function, i.e.,
   \code{theme(axis.text = element_text(size = axis.text.size))}
}
}
\item{\strong{Function \code{test.levene}}}{The \code{plot} function for the
misty object of type \code{"test.levene"} has following plotting arguments:
\itemize{
   \item{\code{geom}}: a character vector indicating which plot elements to draw,
   i.e., \code{"all"} (default) for drawing all plot elements, \code{"violin"}
   for drawing violins, \code{"box"} for drawing boxplots, and \code{"jitter"}
   for drawing jittered data points.
   \item{\code{violin.alpha}}: a numeric value between 0 and 1 (default: \code{0.3})
   indicating the \code{alpha} argument in the \code{geom_violin} function.
   \item{\code{violin.trim}}: a logical value (default: \code{FALSE}), indicating
   the \code{trim} argument in the \code{geom_violin} function.
   \item{\code{box.alpha}}: a numeric value between 0 and 1 (default: \code{0.2})
   indicating the \code{alpha} argument in the \code{geom_boxplot} function.
   \item{\code{box.width}}: a numeric value between 0 and 1 (default: \code{0.2})
   indicating the \code{alpha} argument in the \code{geom_boxplot} function.
   \item{\code{jitter.size}}: a numeric value (default: \code{1.25}) indicating
   the \code{size} argument in the \code{geom_jitter} function.
   \item{\code{jitter.width}}: a numeric value (default: \code{0.05}) indicating
   the \code{width} argument in the \code{geom_jitter} function.
   \item{\code{jitter.height}}: a numeric value (default: \code{0}) indicating
   the \code{height} argument in the \code{geom_jitter} function.
   \item{\code{jitter.alpha}}: a numeric value between 0 and 1 (default: \code{0.2})
   indicating the \code{alpha} argument in the \code{geom_jitter} function.
   \item{\code{gray}}: logical: if \code{TRUE}, the plot is drawn in gray scale.
   \item{\code{start}}: a numeric value between 0 and 1 (default: \code{0.9}),
   indicating the \code{start} argument in the \code{scale_fill_grey} function.
   \item{\code{end}}: a numeric value between 0 and 1 (default: \code{0.4}),
   indicating the \code{end} argument in the \code{scale_fill_grey} function.
   \item{\code{scale.fill.col}}: a character vector (default: \code{NULL}),
   indicating the \code{values} argument in the \code{scale_fill_manual} function.
   \item{\code{xlab}}: a character string (default: \code{NULL}) indicating
   the \code{name} argument in the \code{scale_x_discrete} function.
   \item{\code{ylab}}: a character string (default: \code{NULL}) indicating
   the \code{name} argument in the \code{scale_y_continuous} function.
   \item{\code{ylim}}: a numeric vector with two elements (default: \code{NULL})
   indicating the \code{limits} argument in the \code{scale_y_continuous} function.
   \item{\code{ybreaks}}: a numeric vector (default: \code{waiver()}) indicating
   the \code{breaks} argument in the \code{scale_y_continuous} function.
   \item{\code{title}}: a character string (default: \code{""}) indicating the
   \code{title} argument in the \code{labs} function.
   \item{\code{subtitle}}: a character string (default: \code{""}) indicating
   \code{subtitle} argument in the \code{labs} function.
}
}
 \item{\strong{Function \code{test.linear}}}{
 The \code{plot} function for the misty object of type \code{"test.linear"}
 has following plotting arguments:
 \itemize{
    \item{\code{geom}}: a character vector indicating which plot elements to draw,
    i.e., \code{"all"} (default) for drawing all plot elements, \code{"none"}
    for drawing no regression or smoothing curve, \code{"lm"} for drawing the
    regression line, \code{"loess"} for darwing the LOESS smoothing curve,
    \code{"ci.lm"} for drawing confidence bands around the regression line,
    \code{"ci.loess"} for darwing confidence bands around the LOESS smoothing
    curve. The default setting is \code{c("lm", "loess", "ci.lm", "ci.loess")}.
    \item{\code{point.size}}: a numeric value (default: \code{2}) indicating
    the \code{size} argument in the \code{geom_point} function.
    \item{\code{point.shape}}: a numeric value (default: \code{21}) indicating
    the \code{shape} argument in the \code{geom_point} function.
    \item{\code{point.fill}}: a character string  (default: \code{"#648FFF"})
    indicating the \code{fill} argument in the \code{geom_point} function.
    \item{\code{point.col}}: a character string  (default: \code{"black"})
    indicating the \code{color} argument in the \code{geom_point} function.
    \item{\code{smooth.fill}}: a character vector with two elements (default:
    \code{c("#FE6100", "#56B4E9")}) indicating the \code{fill} argument in the
    \code{geom_smooth} function for the regression and LOESS smoothing curve.
    \item{\code{smooth.alpha}}: a numeric vector with two elements between 0
    and 1 (default: \code{0.15}) indicating the \code{alpha} argument in
    the \code{geom_smooth} function for the regression and LOESS smoothing curve.
    \item{\code{smooth.type}}: a numeric vector with two elements (default:
    \code{c("solid", "dashed")}) indicating the \code{linetype} argument in the
    \code{geom_smooth} function for the regression and LOESS smoothing curve.
    \item{\code{smooth.width}}: a numeric vector with two elements (default:
    \code{c(1, 0.8)}) indicating the \code{linewidth} argument in the
    \code{geom_smooth} function for the regression and LOESS smoothing curve.
    \item{\code{smooth.col}}: a character vector with two elements (default:
    \code{c("#FE6100", "#0057B8")}) indicating the \code{color} argument in
    the \code{geom_smooth} function for the regression and LOESS smoothing curve.
    \item{\code{xlab}}: a character string (default: \code{"Predictor"}) indicating
    the \code{name} argument in the \code{scale_x_continuous} function.
    \item{\code{xlim}}: a numeric vector with two elements (default: \code{NULL})
    indicating the \code{limits} argument in the \code{scale_x_continuous} function.
    \item{\code{xbreaks}}: a numeric vector (default: \code{waiver()}) indicating
    the \code{breaks} argument in the \code{scale_x_continuous} function.
    \item{\code{ylab}}: a character string (default: \code{"Partial Residual"})
    indicating the \code{name} argument in the \code{scale_y_continuous} function.
    \item{\code{ylim}}: a numeric vector with two elements indicating the \code{limits}
    argument in the \code{scale_y_continuous} function.
    \item{\code{ybreaks}}: a numeric vector (default: \code{waiver()}) indicating
    the \code{breaks} argument in the \code{scale_y_continuous} function.
  }
}
\item{\strong{Function \code{test.norm}}}{The \code{plot} function for the
misty object of type \code{"test.norm"} has following plotting arguments:
  \itemize{
    \item{\code{geom}}: a character vector indicating which plot to draw, i.e.,
    \code{"all"} for plotting a Q-Q plot and histogram, \code{"qq"} for plotting
    Q-Q plots, and \code{"hist"} for plotting histograms. Note that \code{"all"}
    and \code{c("qq", "hist")} are valid only when specifying a fitted model
    of class \code{lm} or a single variable. When specifying \code{"all"} or
    \code{c("qq", "hist")} given multiple variables, a multivariate
    Mahalanobis Q-Q plot is drawn. The default setting is \code{c("qq", "hist")}.
    \item{\code{ribbon.fill}}: a character string (default: \code{"#56B4E9"})
    indicating the \code{fill} argument in the \code{geom_ribbon} function.
    \item{\code{ribbon.alpha}}: a numeric value between 0 and 1 (default: \code{0.1})
    indicating the \code{alpha} argument in the \code{geom_ribbon} function.
    \item{\code{ribbon.col}}: a character string (default: \code{"#0057B8"})
    indicating the \code{color} argument in the \code{geom_ribbon} function.
    \item{\code{abline.col}}: a character string (default: \code{"#0057B8"})
    indicating the \code{color} argument in the \code{geom_abline} function.
    \item{\code{abline.width}}: a numeric value (default: \code{0.7}) indicating
    the \code{linewidth} argument in the \code{geom_abline} function.
    \item{\code{point.size}}: a numeric value (default: \code{2}) indicating
    the \code{size} argument in the \code{geom_point} function.
    \item{\code{point.shape}}: a numeric value (default: \code{21}) indicating
    the \code{shape} argument in the \code{geom_point} function.
    \item{\code{point.fill}}: a character string  (default: \code{"#648FFF"})
    indicating the \code{fill} argument in the \code{geom_point} function.
    \item{\code{point.col}}: a character string  (default: \code{"black"})
    indicating the \code{color} argument in the \code{geom_point} function.
    \item{\code{hist.binwidth}}: a numeric value or a function (default: \code{NULL})
    indicating the \code{binwidth} argument in the \code{geom_histogram} function.
    \item{\code{hist.fill}}: a numeric value or a function (default: \code{"gray90"})
    indicating the \code{fill} argument in the \code{geom_histogram} function.
    \item{\code{hist.col}}: a numeric value or a function (default: \code{"black"})
    indicating the \code{color} argument in the \code{geom_histogram} function.
    \item{\code{function.type}}: a numeric or character vector of length 2 (default:
    \code{c("solid", "dashed")}) indicating the \code{linetype} argument in the
    \code{stat_function} function.
    \item{\code{function.width}}: a numeric vector of length 2 (default: \code{c(1, 1))})
    indicating the \code{linewidth} argument in the \code{stat_function} function.
    \item{\code{function.col}}: a character vector of length 2 (default:
    \code{c("#0057B8", "#D55E00")}) indicating the \code{color} argument in
    the \code{stat_function} function.
    \item{\code{xlab}}: a character string indicating the \code{name} argument
    in the \code{scale_x_continuous} function. The default setting is
    \code{"Theoretical Quantiles of the Normal Distribution"} when
    plotting univariate QQ plots and \code{"Chi-Square Quantile"} when plotting
    a multivariate QQ plot.
    \item{\code{xlim}}: a numeric vector with two elements indicating the
    \code{limits} argument in the \code{scale_x_continuous} function.
    \item{\code{xbreaks}}: a numeric vector (default: \code{waiver()}) indicating
    the \code{breaks} argument in the \code{scale_x_continuous} function.
    \item{\code{ylab}}: a character string indicating the \code{name} argument
    in the \code{scale_y_continuous} function The default setting is
    \code{"Sample Quantiles"} when plotting univariate QQ plots and
    \code{"Squared Mahalanobis Distance"} when plotting a multivariate QQ plot.
    \item{\code{ylim}}: a numeric vector with two elements indicating the
    \code{limits} argument in the \code{scale_y_continuous} function.
    \item{\code{ybreaks}}: a numeric vector (default: \code{waiver()}) indicating
    the \code{breaks} argument in the \code{scale_y_continuous} function.
    \item{\code{axis.title.size}}: a numeric value (default: \code{11}) indicating
    the \code{size} argument in the \code{element_text} function, i.e.
    \code{theme(axis.title = element_text(size = axis.text.size))}
    \item{\code{axis.text.size}}: a numeric value (default: 10) indicating the
    \code{size} argument in the \code{element_text} function, i.e.,
    \code{theme(axis.text = element_text(size = axis.text.size))}
    \item{\code{facet.nrow}}: a numeric value (default: \code{NULL}) indicating
    the \code{nrow} argument in the \code{facet_wrap} function when plotting
    multiple Q-Q plots or histograms.
    \item{\code{facet.ncol}}: a numeric value (default: \code{3}) indicating the
    \code{ncol} argument in the \code{facet_wrap} function when plotting multiple
    Q-Q plots or histograms.
    \item{\code{facet.scales}}: a character string indicating \code{scales}
    argument in the \code{facet_wrap} function. The default is \code{"free_y"}
    when plotting multiple Q-Q plots and \code{"free"} when plotting multiple
    histograms.
    \item{\code{strip.text.size}}: a numeric value (default: \code{10}) indicating
    the font size of the strip text, i.e. \code{theme(strip.text = ggplot2::element_text(size = strip.text.size))}.
  }
}
\item{\strong{Functions \code{test.t}, \code{test.welch}, and \code{test.z}}}{The
\code{plot}  function for the misty object of type \code{"test.t"}, \code{"test.welch"},
and \code{"test.z"} has following plotting arguments:
\itemize{
   \item{\code{geom}}: a character string or character vector indicating which
   plot elements to draw, i.e.,  \code{"all"} for drawing all plot elements,
   \code{"bar"} for drawing bars representing means for each groups, \code{"point"}
   for drawing points representing means for each group, \code{"ci"} for drawing
   error bars representing confidence intervals, \code{"jitter"} for drawing
   jittered data points, \code{"line"} for drawing a horizontal line at \code{mu}
   for the one-sample t-test or at \code{0} for the paired-sample t-test.
   \item{\code{conf.level}}: a numeric value between 0 and 1 (default: 0.95)
   indicating the confidence level of the interval.
   \item{\code{adjust}}: logical: if \code{TRUE} (default), difference-adjustment
   for the confidence intervals in a two-sample design is applied.
   \item{\code{point.size}}: a numeric value (default: \code{3}) indicating the
   \code{size} argument in the \code{geom_point} function.
   \item{\code{errorbar.width}}: a numeric value (default: \code{0.1}) indicating
   the \code{width} argument in the \code{geom_errorbar} function.
   \item{\code{hline.type}}: an integer value or character string (default: \code{3})
   indicating the \code{linetype} argument in the \code{geom_hline} function.
   \item{\code{hline.width}}: a numeric value (default: \code{0.8}) indicating
   the \code{linewidth} argument in the \code{geom_hline} function.
   \item{\code{jitter.size}}: a numeric value (default: \code{1.25}) indicating
   the \code{size} argument in the \code{geom_jitter} function.
   \item{\code{jitter.width}}: a numeric value (default: \code{0.05}) indicating
   the \code{width} argument in the \code{geom_jitter} function.
   \item{\code{jitter.height}}: a numeric value (default: \code{0}) indicating
   the \code{height} argument in the \code{geom_jitter} function.
   \item{\code{jitter.alpha}}: a numeric value between 0 and 1 (default: \code{0.1})
   the \code{alpha} argument in the \code{geom_jitter} function.
   \item{\code{xlab}}: a character string (default: \code{NULL}) indicating
   the \code{name} argument in the \code{scale_x_continuous} function.
   \item{\code{ylab}}: a character string (default: \code{"y"}) indicating
   the \code{name} argument in the \code{scale_y_continuous} function.
   \item{\code{ylim}}: a numeric vector with two elements (default: \code{NULL})
   indicating the \code{limits} argument in the \code{scale_y_continuous} function.
   \item{\code{ybreaks}}: a numeric vector (default: \code{waiver()}) indicating
   the \code{breaks} argument in the \code{scale_y_continuous} function.
   \item{\code{title}}: a character string (default: \code{""}) indicating the
   \code{title} argument in the \code{labs} function.
   \item{\code{subtitle}}: a character string (default: \code{""}) indicating the
   \code{subtitle} argument in the \code{labs} function The default setting is
   \code{"Two-Sided Confidence Interval"} when \code{adjust = FALSE} or
   \code{"Two-Sided Difference-Adjusted Confidence Interval"}
   when \code{adjust = TRUE}.
   \item{\code{facet_ncol}}: a numeric value (default: \code{3}) indicating the
   \code{ncol} argument in the \code{facet_wrap} function when conducting
   multiple two-sample t-tests or Welch tests.
   \item{\code{facet_nrow}}: a numeric value (default: \code{NULL}) indicating
   the \code{nrow} argument in the \code{facet_wrap} function when conducting
   multiple two-sample t-tests or Welch tests.
   \item{\code{facet.scales}}: a character string (default: \code{"free_y"})
   indicating the \code{scales} argument in the \code{facet_wrap} function when
   conducting multiple two-sample t-tests or Welch tests.
}
}
}
}
\author{
Takuya Yanagida
}
