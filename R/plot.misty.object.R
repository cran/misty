#' Plots misty.object object
#'
#' This function plots an \code{misty.object} object.
#'
#' @param x                    \code{misty.object} object.
#' @param plot                 see 'Details'.
#' @param geom                 see 'Details'.
#' @param color                see 'Details'.
#' @param type                 see 'Details'.
#' @param line.type            see 'Details'.
#' @param line.width           see 'Details'.
#' @param line.col             see 'Details'.
#' @param ci.col               see 'Details'.
#' @param ci.width             see 'Details'.
#' @param ci.type              see 'Details'.
#' @param errorbar.width       see 'Details'.
#' @param errorbar.dodge.width see 'Details'.
#' @param smooth.fill          see 'Details'.
#' @param smooth.alpha         see 'Details'.
#' @param smooth.type          see 'Details'.
#' @param smooth.width         see 'Details'.
#' @param smooth.col           see 'Details'.
#' @param point.size           see 'Details'.
#' @param point.width          see 'Details'.
#' @param point.type           see 'Details'.
#' @param point.shape          see 'Details'.
#' @param point.col            see 'Details'.
#' @param point.fill           see 'Details'.
#' @param jitter.size          see 'Details'.
#' @param jitter.width         see 'Details'.
#' @param jitter.height        see 'Details'.
#' @param jitter.alpha         see 'Details'.
#' @param ribbon.fill          see 'Details'.
#' @param ribbon.col           see 'Details'.
#' @param ribbon.alpha         see 'Details'.
#' @param violin.alpha         see 'Details'.
#' @param violin.trim          see 'Details'.
#' @param density.col          see 'Details'.
#' @param density.width        see 'Details'.
#' @param density.type         see 'Details'.
#' @param density.alpha        see 'Details'.
#' @param hist.binwidth        see 'Details'.
#' @param hist.bins            see 'Details'.
#' @param hist.col             see 'Details'.
#' @param hist.fill            see 'Details'.
#' @param hist.alpha           see 'Details'.
#' @param box.width            see 'Details'.
#' @param box.alpha            see 'Details'.
#' @param tile.col             see 'Details'.
#' @param tile.alpha           see 'Details'.
#' @param function.type        see 'Details'.
#' @param function.width       see 'Details'.
#' @param function.col         see 'Details'.
#' @param abline.col           see 'Details'.
#' @param abline.width         see 'Details'.
#' @param hline.intercept      see 'Details'.
#' @param hline.type           see 'Details'.
#' @param hline.width          see 'Details'.
#' @param hline.col            see 'Details'.
#' @param vline.intercept      see 'Details'.
#' @param vline.type           see 'Details'.
#' @param vline.width          see 'Details'.
#' @param vline.col            see 'Details'.
#' @param text.size            see 'Details'.
#' @param text.col             see 'Details'.
#' @param text.font            see 'Details'.
#' @param gray                 see 'Details'.
#' @param start                see 'Details'.
#' @param end                  see 'Details'.
#' @param xlab                 see 'Details'.
#' @param ylab                 see 'Details'.
#' @param xlim                 see 'Details'.
#' @param ylim                 see 'Details'.
#' @param xbreaks              see 'Details'.
#' @param ybreaks              see 'Details'.
#' @param axis.title.size      see 'Details'.
#' @param axis.text.size       see 'Details'.
#' @param strip.text.size      see 'Details'.
#' @param scale.fill.col       see 'Details'.
#' @param scale.fill.low       see 'Details'.
#' @param scale.fill.high      see 'Details'.
#' @param scale.fill.mid       see 'Details'.
#' @param title                see 'Details'.
#' @param subtitle             see 'Details'.
#' @param group.col            see 'Details'.
#' @param plot.margin          see 'Details'.
#' @param legend.title         see 'Details'.
#' @param legend.position      see 'Details'.
#' @param legend.box.margin    see 'Details'.
#' @param legend.key.size      see 'Details'.
#' @param legend.text.size     see 'Details'.
#' @param facet.ncol           see 'Details'.
#' @param facet.nrow           see 'Details'.
#' @param facet.scales         see 'Details'.
#' @param conf.level           see 'Details'.
#' @param adjust               see 'Details'.
#' @param filename             a character string indicating the \code{filename}
#'                             argument including the file extension in the \code{ggsave}
#'                             function. Note that one of \code{".eps"}, \code{".ps"},
#'                             \code{".tex"}, \code{".pdf"} (default), \code{".jpeg"},
#'                             \code{".tiff"}, \code{".png"}, \code{".bmp"},
#'                             \code{".svg"} or \code{".wmf"} needs to be specified
#'                             as file extension in the \code{file} argument.
#' @param width                a numeric value indicating the \code{width} argument
#'                             for the \code{ggsave} function.
#' @param height               a numeric value indicating the \code{height} argument
#'                             for the \code{ggsave} function.
#' @param units                a character string indicating the \code{units} argument
#'                             in the \code{ggsave} function. Note that one of
#'                             \code{"in"}, \code{"cm"}, \code{"mm"}, or \code{"px"}
#'                             needs to be specified.
#' @param dpi                  a numeric value indicating the \code{dpi} argument
#'                             for the \code{ggsave} function.
#' @param check                logical: if \code{TRUE} (default), argument specification
#'                             is checked.
#'
#' @details
#' This function provides plotting arguments depending on the type of the output
#' object specified for the argument \code{x}. Note that the arguments \code{filename},
#' \code{width}, \code{height}, \code{units}, and \code{dpi} are available for
#' all types of the output object.
#' \describe{
#' \item{\strong{Functions \code{aov.b} and \code{aov.w}}}{The \code{plot} function
#' for the misty object of type \code{"aov.b"}, \code{"aov.w"}, and \code{test.welch}
#' has following plotting arguments:
#' \itemize{
#'    \item{\code{geom}}: a character vector indicating which plot elements to draw,
#'    i.e., \code{"all"} for drawing all plot elements, \code{"bar"} for drawing
#'    bars representing means for each groups, \code{"point"} for drawing points
#'    representing means for each group, \code{"line"} for drawing a line connecting
#'    means of each group and lines connecting data points when requesting \code{"jitter"},
#'    \code{"ci"} for drawing error bars representing confidence intervals, and
#'    \code{"jitter"} for drawing jittered data points.
#'    \item{\code{conf.level}}: a numeric value between 0 and 1 (default: \code{0.95})
#'    indicating the confidence level of the interval.
#'    \item{\code{adjust}}: logical: if \code{TRUE} (default), difference-adjustment
#'    for the confidence intervals is applied.
#'    \item{\code{point.size}}: a numeric value (default: \code{3}) indicating the
#'    \code{size} argument in the \code{geom_point} function.
#'    \item{\code{line.type}}: a numeric value (default: \code{"solid"}) indicating
#'    the \code{linetype} argument for the \code{geom_line} function.
#'    Note that this argument is only available for the \code{"aov.w"} function.
#'    \item{\code{line.width}}: a numeric value (default: \code{0.5}) indicating
#'    the \code{linewidth} argument for the \code{geom_line} function.
#'    Note that this argument is only available for the \code{"aov.w"} function.
#'    \item{\code{errorbar.width}}: a numeric value (default: \code{0.1}) indicating
#'    the \code{width} argument for the \code{geom_errorbar} function.
#'    \item{\code{jitter.size}}: a numeric value (default: \code{1.25}) indicating
#'    the \code{size} argument for the \code{geom_jitter} function.
#'    \item{\code{jitter.width}}: a numeric value (default: \code{0.05}) indicating
#'    the \code{width} argument for the \code{geom_jitter} function.
#'    \item{\code{jitter.height}}: a numeric value (default: \code{0}) indicating
#'    the \code{height} argument for the \code{geom_jitter} function
#'    \item{\code{jitter.alpha}}: a numeric value between 0 and 1 (default: \code{0.1})
#'    the \code{alpha} argument for the \code{geom_jitter} function
#'    \item{\code{xlab}}: a character string (default: \code{NULL}) indicating
#'    the \code{name} argument in the \code{scale_x_discrete} function.
#'    \item{\code{ylab}}: a character string (default: \code{"y"}) indicating
#'    the \code{name} argument in the \code{scale_y_continuous} function.
#'    \item{\code{ylim}}: a numeric vector with two elements (default: \code{NULL})
#'    indicating the \code{limits} argument in the \code{scale_y_continuous} function.
#'    \item{\code{ybreaks}}: a numeric vector (default: \code{waiver()}) indicating
#'    the \code{breaks} argument in the \code{scale_y_continuous} function.
#'    \item{\code{title}}: a character string (default: \code{""}) indicating the
#'    \code{title} argument in the \code{labs} function.
#'    \item{\code{subtitle}}: a character string indicating the \code{subtitle}
#'    argument in the \code{labs} function. The default is \code{"Two-Sided Confidence Interval"}
#'    when \code{adjust = FALSE} or \code{"Two-Sided Difference-Adjusted Confidence Interval"}
#'    for the \code{aov.b} function and
#'    \code{"Two-Sided Difference-Adjusted Cousineau-Morey Confidence Interval Confidence Interval"}
#'    for the \code{aov.w} function when \code{adjust = TRUE}.
#' }
#' }
#' \item{\strong{Function \code{boot.bs}}}{The ggplot2 arguments provided for
#'   the function \code{boot.bs} are described in the help page of the
#'   \code{\link{boot.bs}} function.
#' }
#' \item{\strong{Function \code{check.outlier}}}{The \code{plot} function for the
#' misty object of type \code{"check.outlier"} has following plotting arguments:
#' \itemize{
#'    \item{\code{type}}:  character vector indicating which statistical measures to
#'    include in the plot, i.e., \code{"mahal"} for the Mahalanobis distance,
#'    \code{"hat"} for the hat value, \code{"rstand"} for the standardized
#'    leverage-corrected residual, \code{"rstud"} for the studentized leverage-corrected
#'    residual, \code{"cook"} for the Cook's distance, and \code{"dfb"} for the
#'    DFBETAS.
#'    \item{\code{geom}}: a character vector indicating which plot elements to draw,
#'    i.e., \code{"none"} for not drawing any labels, \code{"text"} (default)
#'    for drawing labels above or below each observation.
#'    \item{\code{point.size}}: a numeric value (default: \code{2}) indicating
#'    the \code{size} argument in the \code{geom_point} function.
#'    \item{\code{point.shape}}: a numeric value (default: \code{21}) indicating
#'    the \code{shape} argument in the \code{geom_point} function.
#'    \item{\code{point.fill}}: a character string  (default: \code{"#648FFF"})
#'    indicating the \code{fill} argument in the \code{geom_point} function.
#'    \item{\code{point.col}}: a character string  (default: \code{"black"})
#'    indicating the \code{color} argument in the \code{geom_point} function.
#'    \item{\code{line.type}}: an integer value or character string (default:
#'    \code{"solid"}) indicating the \code{linetype} argument in the \code{geom_linerange}
#'    function.
#'    \item{\code{line.width}}: a numeric value (default: \code{0.5}) indicating
#'    the \code{linewidth} argument in the \code{geom_linerange} function.
#'    \item{\code{line.col}}: a character string (default: \code{"#648FFF"}) indicating
#'    the \code{color} argument in the \code{geom_linerange} function.
#'    \item{\code{hline.type}}: an integer value or character string (default:
#'    \code{"dashed"}) indicating the \code{linetype} argument in the \code{geom_hline}
#'    function.
#'    \item{\code{hline.width}}: a numeric value (default: \code{0.5}) indicating
#'    the \code{linewidth} argument in the \code{geom_hline} function.
#'    \item{\code{hline.col}}: a character string (default: \code{"#FE6100"}) indicating
#'    the \code{color} argument in the \code{geom_hline} function.
#'    the \code{color} argument in the \code{geom_hline} function.
#'    \item{\code{text.size}}: a numeric value (default: \code{3}) indicating the
#'    \code{size} argument in the \code{geom_text} function.
#'    \item{\code{text.col}}: a character string (default: \code{"black"}) indicating
#'    the \code{color} argument in the \code{geom_text} function.
#'    \item{\code{ylim}}: a numeric vector with two elements indicating the \code{limits}
#'    argument in the \code{scale_y_continuous} function.
#'    \item{\code{ybreaks}}: a numeric vector (default: \code{waiver()}) indicating
#'    the \code{breaks} argument in the \code{scale_y_continuous} function.
#'    \item{\code{axis.title.size}}: a numeric value (default: \code{11}) indicating
#'    the \code{size} argument in the \code{element_text} function, i.e.
#'    \code{theme(axis.title = element_text(size = axis.text.size))}
#'    \item{\code{axis.text.size}}: a numeric value (default: 10) indicating the
#'    \code{size} argument in the \code{element_text} function, i.e.,
#'    \code{theme(axis.text = element_text(size = axis.text.size))}
#' }
#' }
#' \item{\strong{Function \code{check.resid}}}{The \code{plot} function for the
#' misty object of type \code{"check.resid"} have the same arguments as the
#' \code{test.linear} function when \code{type = "linear"}, the \code{test.homo}
#' function when \code{type = "homo"}, and the \code{test.normal} function when
#' \code{type = "test.norm"}. In addition, the \code{plot} function for the
#' misty object of type \code{"check.resid"} has following plotting argument:
#' \itemize{
#'    \item{\code{geom}}: a character vector indicating which plot elements to
#'    draw, i.e., \code{"all"} for drawing all plot elements, \code{"lm"} for
#'    drawing the regression line, \code{"loess"} for drawing the LOESS smoothing
#'    curve, \code{"ci.lm"} for drawing confidence bands around the regression
#'    line, \code{"ci.loess"} for drawing confidence bands around the LOESS smoothing
#'    curve, \code{"hline"} or drawing a horizontal line at \code{0}, \code{"qq"}
#'    for plotting Q-Q plots, and \code{"hist"} for plotting histograms. Note that
#'    the arguments \code{"lm"}, \code{"loess"}, \code{"ci.lm"}, and \code{"ci.less"}
#'    are applicable when \code{type = "linear"}, the arguments \code{"hline"},
#'    \code{"loess"}, and \code{"ci.loess"} are applicable when \code{type = "homo"},
#'    and the arguments \code{"lqq"} and \code{"hist"} are applicable when
#'    \code{type = "norm"}.
#' }
#' }
#' \item{\strong{Functions \code{ci.*}}}{The \code{plot} function for the misty
#' object of type \code{"ci.cor"}, \code{"ci.mean"}, \code{"ci.median"},
#' \code{"ci.prop"}, \code{"ci.var"}, and \code{"ci.sd"} has following plotting
#' arguments:
#' \itemize{
#'    \item{\code{plot}}: a character string indicating the type of the plot
#'    to display, i.e., \code{"ci"} (default) for displaying confidence intervals
#'    or \code{"boot"} for displaying bootstrap samples with histograms and density
#'    curves when the argument.
#'    \item{\code{geom}}: a character vector indicating which plot elements to draw,
#'    i.e., \code{"all"} for drawing all plot elements,\code{"hist"} for drawing histograms
#'    when plotting bootstrap samples (\code{plot = "boot"}), \code{"density"}
#'    for drawing density curves when plotting bootstrap samples (\code{plot = "boot"}),
#'    \code{"point"} for drawing vertical lines representing the point estimate
#'    of the correlation coefficients when plotting bootstrap samples
#'    (\code{plot = "boot"}), \code{"ci"} for drawing vertical lines representing
#'    the bootstrap confidence intervals of the correlation when plotting bootstrap
#'    samples (\code{plot = "boot"}), and \code{"line"} for drawing a horizontal
#'    line when specifying \code{plot = "ci"} or a vertical line when specifying
#'    \code{plot = "boot"}.
#'    \item{\code{point.size}}: a numeric value (default: \code{2.5}) indicating
#'    the \code{size} argument in the \code{geom_point} function.
#'    \item{\code{point.shape}}: a numeric value between 0 and 25 (default: \code{19})
#'    indicating the \code{shape} argument in the \code{geom_point} function.
#'    \item{\code{errorbar.width}}: a numeric value (default: \code{0.3}) indicating
#'    the \code{width} argument in the \code{geom_errorbar} function.
#'    \item{\code{errorbar.dodge.width}}: a numeric value (default: \code{0.5}) indicating
#'    the \code{width} argument in the \code{geom_errorbar} function.
#'    \item{\code{hist.bins}}: a numeric value or a function (default: \code{NULL})
#'    indicating the \code{bins} argument in the \code{geom_histogram} function.
#'    \item{\code{hist.binwidth}}: a numeric value or a function (default: \code{NULL})
#'    indicating the \code{binwidth} argument in the \code{geom_histogram} function.
#'    \item{\code{hist.alpha}}: a numeric value between 0 and 1 (default: \code{0.4})
#'    indicating the \code{alpha} argument in the \code{geom_histogram}
#'    function.
#'    \item{\code{hist.fill}}: a character string (default: \code{"gray85"}) indicating
#'    the \code{fill} argument in the \code{geom_histogram} function.
#'    \item{\code{density.col}}: a character string (default: \code{"#0072B2"})
#'    indicating the \code{color} argument in the \code{geom_density} function.
#'    \item{\code{density.width}}: a numeric value (default: \code{0.5}) indicating
#'    the \code{linewidth} argument in the \code{geom_density} function.
#'    \item{\code{density.type}}: a numeric value or character string (default:
#'    \code{0.5}) indicating the \code{linetype} argument in the \code{geom_density}
#'    function.
#'    \item{\code{point.col}}: a character string (default: \code{"#CC79A7"}) indicating
#'    the \code{color} argument in the \code{geom_vline} function. Note that this
#'    argument applied only when no grouping variable was specified \code{group = NULL}.
#'    \item{\code{point.width}}: a numeric value (default: \code{0.6}) indicating
#'    the \code{linewdith} argument in the \code{geom_vline} function.
#'    \item{\code{point.type}}: a numeric value or character string (default:
#'    \code{"solid"}) indicating the \code{linetype} argument in the \code{geom_vline}
#'    function.
#'    \item{\code{ci.col}}: character string (default: \code{"black"}) indicating the
#'    \code{color} argument in the \code{geom_vline} function. Note that this argument
#'    applied only when no grouping variable was specified \code{group = NULL}.
#'    \item{\code{ci.width}}: a numeric value (default: \code{0.6}) indicating
#'    the \code{linewdith} argument in the \code{geom_vline} function.
#'    \item{\code{ci.type}}: a numeric value or character string (default:
#'    \code{"dashed"}) indicating the \code{linetype} argument in the \code{geom_vline}
#'    function.
#'    \item{\code{hline.intercept}}: a numeric value (default = \code{0}) indicating the
#'    \code{yintercept} argument in the \code{geom_hline} function.
#'    \item{\code{hline.type}}: a character string (default: \code{"solid"}) indicating
#'    the \code{linetype} argument in the \code{geom_hline} function.
#'    \item{\code{hline.col}}: a character string (default: \code{"gray65"}) indicating
#'    the \code{color} argument in the \code{geom_hline} function.
#'    \item{\code{vline.intercept}}: a numeric value (default = \code{0}) indicating the
#'    \code{xintercept} argument in the \code{geom_vline} function.
#'    \item{\code{vline.type}}: a character string (default: \code{"solid"}) indicating
#'    the \code{linetype} argument in the \code{geom_vline} function.
#'    \item{\code{vline.col}}: a character string (default: \code{"gray65"}) indicating
#'    the \code{color} argument in the \code{geom_vline} function.
#'    \item{\code{xlab}}: a character string indicating the \code{name} argument
#'    in the \code{scale_x_continuous} function. The default setting is \code{xlab = NULL}
#'    when \code{plot = "ci"} and    \code{xlab = "Correlation Coefficient"},
#'    \code{xlab = "Arithmetic Mean"}, \code{xlab = "Median"}, \code{xlab = "Proportion"},
#'    \code{xlab = "Variance"}, or \code{xlab = "Standard Deviation"}.
#'    \item{\code{ylab}}: a character string indicating the \code{name} argument
#'    in the \code{scale_y_continuous} function. The default setting is
#'    \code{ylab = "Correlation Coefficient"}, \code{ylab = "Arithmetic Mean"},
#'    \code{ylab = "Median"}, \code{ylab = "Proportion"}, \code{ylab = "Variance"},
#'    or \code{ylab = "Standard Deviation"} when \code{plot = "ci"} and
#'    \code{ylab = "Probability Density f(x)"} when \code{plot = "boot"}.
#'    \item{\code{xlim}}: a numeric vector with two elements indicating the
#'    \code{limits} argument in the \code{scale_x_continuous} function. The default
#'    setting is \code{xlim = NULL} when \code{plot = "ci"} and \code{xlim = c(-1, 1)}
#'    for the correlation coefficient and proportion or \code{xlim = NULL)} for
#'    the arithmetic mean, median, variance and standard deviation when \code{plot = "boot"}.
#'    \item{\code{ylim}}: a numeric vector with two elements indicating the
#'    \code{limits} argument in the \code{scale_y_continuous} function. The default
#'    setting is \code{ylim = c(-1, 1)} for the correlation coefficient and proportion
#'    and \code{ylim = NULL} for the arithmetic mean, median, variance and standard
#'    deviation when \code{plot = "ci"} and \code{xlim = NULL} when \code{plot = "boot"}.
#'    \item{\code{xbreaks}}: a numeric vector (default: \code{waiver()}) indicating
#'    the \code{breaks} argument in the \code{scale_x_continuous} function.
#'    \item{\code{ybreaks}}: a numeric vector (default: \code{waiver()}) indicating
#'    the \code{breaks} argument in the \code{scale_y_continuous} function.
#'    \item{\code{axis.title.size}}: a numeric value (default: \code{11}) indicating
#'    the \code{size} argument in the \code{element_text} function, i.e.
#'    \code{theme(axis.title = element_text(size = axis.text.size))}
#'    \item{\code{axis.text.size}}: a numeric value (default: 10) indicating the
#'    \code{size} argument in the \code{element_text} function,
#'    i.e. \code{theme(axis.text = element_text(size = axis.text.size))}.
#'    \item{\code{strip.text.size}}: a numeric value (default: \code{11}) indicating
#'    the \code{size} argument in the \code{element_text} function, i.e.
#'    \code{theme(strip.text = element_text(size = strip.text.size))}.
#'    \item{\code{title}}: a character string (default: \code{NULL}) indicating the
#'    \code{title} argument in the \code{labs} function.
#'    \item{\code{subtitle}}: a character string (default: \code{NULL}) indicating
#'    the \code{subtite} argument in the \code{labs} function.
#'    \item{\code{group.col}}: a character vector (default: \code{NULL}) indicating
#'    the \code{color} argument in the \code{scale_color_manual} and \code{scale_fill_manual}
#'    functions when specifying a grouping variable using the argument \code{group}.
#'    \item{\code{plot.margin}}: a numeric vector (default: \code{NA}) with four
#'    elements indicating the \code{plot.margin} argument in the \code{theme} function.
#'    The default setting is \code{c(5.5, 5.5, 5.5, 5.5)} but switches to
#'    \code{c(5.5, 5.5, -2.5, 5.5)} when specifying a grouping variable using
#'    the argument \code{group}.
#'    \item{\code{legend.title}}: a character string (default: \code{""}) indicating
#'    the \code{color} argument in the \code{labs} function when specifying a
#'    grouping variable using the argument \code{group}.
#'    \item{\code{legend.position}}: a character string (default: \code{"bottom"})
#'    indicating the \code{legend.position} in the \code{theme} argument when
#'    specifying a grouping variable using the argument \code{group}.
#'    \item{\code{legend.box.margin}}: a numeric vector (default: \code{c(-10, 0, 0, 0)})
#'    with four elements indicating the \code{legend.box.margin} argument in the
#'    \code{theme} function when specifying a grouping variable using the argument
#'    \code{group}.
#'    \item{\code{facet.ncol}}: a numeric value (default: \code{NULL}) indicating the
#'    \code{ncol} argument in the \code{facet_wrap} when specifying a split variable
#'    using the argument \code{split}.
#'    \item{\code{facet.nrow}}: a numeric value (default: \code{NULL}) indicating the
#'    \code{nrow} argument in the \code{facet_wrap} function when specifying a
#'    split variable using the argument \code{split}.
#'    \item{\code{facet.scales}}: a character string (default: \code{"free_y"})
#'    indicating the \code{scales} argument in the \code{facet_wrap} function,
#'    i.e. \code{"fixed"}, \code{"free_x"}, \code{"free_y"} (default) or \code{"free"}
#'    when specifying a split variable using the argument \code{split}.
#' }
#' }
#' \item{\strong{Functions \code{cor.matrix} and \code{multilevel.cor}}}{
#' The \code{plot} functions for the misty object of type \code{"cor.matrix"}
#' and \code{"multilevel.cor"} function have  following plotting arguments:
#' \itemize{
#'    \item{\code{tile.col}}: a character string (default: \code{"black"})
#'    indicating the \code{color} argument in the \code{geom_tile} function.
#'    \item{\code{scale.fill.low}}: a character string (default: \code{"#D41159"})
#'    indicating the \code{low} argument in the \code{scale_fill_gradient2} function.
#'    \item{\code{scale.fill.high}}: a character string (default: \code{"#1A85FF"})
#'    indicating the \code{high} argument in the \code{scale_fill_gradient2} function.
#'    \item{\code{scale.fill.mid}}: a character string (default: \code{"#1A85FF"})
#'    indicating the \code{mid} argument in the \code{scale_fill_gradient2} function.
#'    \item{\code{text.size}}: a numeric vector with two elements (default: \code{c(3.5, 3.5)})
#'    indicating the \code{size} argument in the \code{geom_text} function.
#'    \item{\code{text.col}}: a character vector with two elements (default: \code{c("black", "black")})
#'    indicating the \code{color} argument in the \code{geom_text} function.
#'    \item{\code{text.font}}: a character vector with two elements (default: \code{c("plain", "plain")})
#'    indicating the \code{fontface} argument in the \code{geom_text} function.
#' }
#' }
#' \item{\strong{Function \code{item.dfi}}}{The ggplot2 arguments provided for
#'   the function \code{item.dfi} are described in the help page of the
#'   \code{\link{item.dfi}} function.
#' }
#' \item{\strong{Functions \code{multilevel.r2} and \code{multilevel.r2.manual}}}{
#' The \code{plot} functions for the misty object of type \code{"multilevel.r2"}
#' and \code{"multilevel.r2.manual"} have following plotting arguments:
#' \itemize{
#'    \item{\code{scale.fill.col}}: a character vector, graphical parameter indicating
#'    the color of bars in the bar chart in the following order: Fixed slopes (Within),
#'    Fixed slopes (Between), Slope variation (Within), Intercept variation
#'    (Between), and Residual (Within). The default setting is
#'    \code{c("#D55E00", "#0072B2", "#CC79A7", "#009E73", "#E69F00")}.
#'    \item{\code{gray}}: logical: if \code{TRUE}, graphical parameter to draw
#'    the bar chart in gray scale.
#'    \item{\code{start}}: a numeric value between 0 and 1 (default: \code{0.15}),
#'    graphical parameter to specify the gray value at the low end of the palette.
#'    \item{\code{end}}: a numeric value between 0 and 1 (default: \code{0.85}),
#'    graphical parameter to specify the gray value at the high end of the palette.
#' }
#' }
#' \item{\strong{Function \code{na.pattern}}}{
#' The \code{plot} functions for the misty object of type \code{"na.pattern"}
#' function have following plotting arguments:
#' \itemize{
#'    \item{\code{geom}}: a character vector indicating which plot elements to
#'    draw, i.e., \code{"all"} for drawing all plot elements, \code{"square"} (default)
#'    for drawing squares that mimic the \code{md.pattern} function in the package
#'    \pkg{mice}, and \code{"rotate"}, to rotate variable name labels 90 degrees.
#'    \item{\code{tile.alpha}}: a numeric value between 0 and 1 (default: \code{0.6})
#'    for the \code{alpha} argument (default is \code{0.1}).
#'    \item{\code{scale.fill.col}}: a character string indicating the color for the
#'    \code{"fill"} argument. Note that the first color represents missing values
#'    and the second color represent observed values. The default setting is
#'    \code{c("#B61A51B3", "#006CC2B3")}.
#'    \item{\code{plot.margin}}: a numeric vector indicating the \code{plot.margin}
#'    argument for the \code{margin} function for the \code{plot.margin} argument
#'    within the \code{theme} function. The default setting is \code{c(4, 16, 0, 4)}.
#'    \item{\code{legend.box.margin}}: a numeric vector indicating the \code{legend.box.margin}
#'    argument for the \code{element_text} function for the \code{legend.text}
#'    argument within the \code{theme} function. The default setting is
#'    \code{c(-8, 6, 6, 6)}.
#'    \item{\code{legend.key.size}}: a numeric value (default: \code{unit(12, "pt")})
#'    indicating  the \code{legend.key} argument for the \code{theme} function.
#' }
#' }
#' \item{\strong{Function \code{test.homo}}}{The \code{plot} function for the
#' misty object of type \code{"test.homo"} has following plotting arguments:
#' \itemize{
#'    \item{\code{geom}}: a character vector indicating which plot elements to draw,
#'    i.e., \code{"all"} (default) for drawing all plot elements, \code{"none"}
#'    for drawing no horizontal or smoothing curve, \code{"hline"} for drawing a
#'    horizontal line at \code{0} using the function \code{ggplot2::geom_hline},
#'    \code{"loess"} for drawing the LOESS smoothing curve using the function
#'    \code{ggplot2::geom_smooth}, and \code{"ci.less"} for drawing confidence bands
#'    around the LESS smoothing curve (i.e., \code{se = TRUE}).
#'    \item{\code{hline.type}}: a numeric value (default: \code{"solid"})
#'    indicating the \code{linetype} argument in the \code{geom_hline} function.
#'    \item{\code{hline.width}}: a numeric value (default: \code{1})
#'    indicating the \code{linewidth} argument in the \code{geom_hline} function.
#'    \item{\code{hline.col}}: a character string (default: \code{"#FE6100"})
#'    indicating the \code{color} argument in the \code{geom_hline} function.
#'    \item{\code{point.size}}: a numeric value (default: \code{2}) indicating
#'    the \code{size} argument in the \code{geom_point} function.
#'    \item{\code{point.shape}}: a numeric value (default: \code{21}) indicating
#'    the \code{shape} argument in the \code{geom_point} function.
#'    \item{\code{point.fill}}: a character string  (default: \code{"#648FFF"})
#'    indicating the \code{fill} argument in the \code{geom_point} function.
#'    \item{\code{point.col}}: a character string  (default: \code{"black"})
#'    indicating the \code{color} argument in the \code{geom_point} function.
#'    \item{\code{smooth.fill}}: a character string (default: \code{"##56B4E9"})
#'    indicating the \code{fill} argument in the \code{geom_smooth} function.
#'    \item{\code{smooth.alpha}}: a numeric value between 0 and 1 (default: \code{0.15})
#'    indicating the \code{alpha} argument in the \code{geom_smooth} function.
#'    \item{\code{smooth.type}}: a numeric value (default: \code{"dashed"})
#'    indicating the \code{linetype} argument in the \code{geom_smooth} function.
#'    \item{\code{smooth.width}}: a numeric value (default: \code{1})
#'    indicating the \code{linewidth} argument in the \code{geom_smooth} function.
#'    \item{\code{smooth.col}}: a character string (default: \code{"#0057B8"})
#'    indicating the \code{color} argument in the \code{geom_smooth} function.
#'    \item{\code{xlim}}: a numeric vector with two elements indicating the \code{limits}
#'    argument in the \code{scale_x_continuous} function.
#'    \item{\code{xbreaks}}: a numeric vector (default: \code{waiver()}) indicating
#'    the \code{breaks} argument in the \code{scale_x_continuous} function.
#'    \item{\code{ylim}}: a numeric vector with two elements indicating the \code{limits}
#'    argument in the \code{scale_y_continuous} function.
#'    \item{\code{ybreaks}}: a numeric vector (default: \code{waiver()}) indicating
#'    the \code{breaks} argument in the \code{scale_y_continuous} function.
#'    \item{\code{axis.title.size}}: a numeric value (default: \code{11}) indicating
#'    the \code{size} argument in the \code{element_text} function, i.e.
#'    \code{theme(axis.title = element_text(size = axis.text.size))}
#'    \item{\code{axis.text.size}}: a numeric value (default: 10) indicating the
#'    \code{size} argument in the \code{element_text} function, i.e.,
#'    \code{theme(axis.text = element_text(size = axis.text.size))}
#' }
#' }
#' \item{\strong{Function \code{test.levene}}}{The \code{plot} function for the
#' misty object of type \code{"test.levene"} has following plotting arguments:
#' \itemize{
#'    \item{\code{geom}}: a character vector indicating which plot elements to draw,
#'    i.e., \code{"all"} (default) for drawing all plot elements, \code{"violin"}
#'    for drawing violins, \code{"box"} for drawing boxplots, and \code{"jitter"}
#'    for drawing jittered data points.
#'    \item{\code{violin.alpha}}: a numeric value between 0 and 1 (default: \code{0.3})
#'    indicating the \code{alpha} argument in the \code{geom_violin} function.
#'    \item{\code{violin.trim}}: a logical value (default: \code{FALSE}), indicating
#'    the \code{trim} argument in the \code{geom_violin} function.
#'    \item{\code{box.alpha}}: a numeric value between 0 and 1 (default: \code{0.2})
#'    indicating the \code{alpha} argument in the \code{geom_boxplot} function.
#'    \item{\code{box.width}}: a numeric value between 0 and 1 (default: \code{0.2})
#'    indicating the \code{alpha} argument in the \code{geom_boxplot} function.
#'    \item{\code{jitter.size}}: a numeric value (default: \code{1.25}) indicating
#'    the \code{size} argument in the \code{geom_jitter} function.
#'    \item{\code{jitter.width}}: a numeric value (default: \code{0.05}) indicating
#'    the \code{width} argument in the \code{geom_jitter} function.
#'    \item{\code{jitter.height}}: a numeric value (default: \code{0}) indicating
#'    the \code{height} argument in the \code{geom_jitter} function.
#'    \item{\code{jitter.alpha}}: a numeric value between 0 and 1 (default: \code{0.2})
#'    indicating the \code{alpha} argument in the \code{geom_jitter} function.
#'    \item{\code{gray}}: logical: if \code{TRUE}, the plot is drawn in gray scale.
#'    \item{\code{start}}: a numeric value between 0 and 1 (default: \code{0.9}),
#'    indicating the \code{start} argument in the \code{scale_fill_grey} function.
#'    \item{\code{end}}: a numeric value between 0 and 1 (default: \code{0.4}),
#'    indicating the \code{end} argument in the \code{scale_fill_grey} function.
#'    \item{\code{scale.fill.col}}: a character vector (default: \code{NULL}),
#'    indicating the \code{values} argument in the \code{scale_fill_manual} function.
#'    \item{\code{xlab}}: a character string (default: \code{NULL}) indicating
#'    the \code{name} argument in the \code{scale_x_discrete} function.
#'    \item{\code{ylab}}: a character string (default: \code{NULL}) indicating
#'    the \code{name} argument in the \code{scale_y_continuous} function.
#'    \item{\code{ylim}}: a numeric vector with two elements (default: \code{NULL})
#'    indicating the \code{limits} argument in the \code{scale_y_continuous} function.
#'    \item{\code{ybreaks}}: a numeric vector (default: \code{waiver()}) indicating
#'    the \code{breaks} argument in the \code{scale_y_continuous} function.
#'    \item{\code{title}}: a character string (default: \code{""}) indicating the
#'    \code{title} argument in the \code{labs} function.
#'    \item{\code{subtitle}}: a character string (default: \code{""}) indicating
#'    \code{subtitle} argument in the \code{labs} function.
#' }
#' }
#'  \item{\strong{Function \code{test.linear}}}{The \code{plot} function for the
#'  misty object of type \code{"test.linear"} has following plotting arguments:
#'  \itemize{
#'     \item{\code{geom}}: a character vector indicating which plot elements to draw,
#'     i.e., \code{"all"} (default) for drawing all plot elements, \code{"none"}
#'     for drawing no regression or smoothing curve, \code{"lm"} for drawing the
#'     regression line, \code{"loess"} for darwing the LOESS smoothing curve,
#'     \code{"ci.lm"} for drawing confidence bands around the regression line,
#'     \code{"ci.loess"} for darwing confidence bands around the LOESS smoothing
#'     curve The default setting is \code{c("lm", "loess", "ci.lm", "ci.loess")}.
#'     \item{\code{point.size}}: a numeric value (default: \code{2}) indicating
#'     the \code{size} argument in the \code{geom_point} function.
#'     \item{\code{point.shape}}: a numeric value (default: \code{21}) indicating
#'     the \code{shape} argument in the \code{geom_point} function.
#'     \item{\code{point.fill}}: a character string  (default: \code{"#648FFF"})
#'     indicating the \code{fill} argument in the \code{geom_point} function.
#'     \item{\code{point.col}}: a character string  (default: \code{"black"})
#'     indicating the \code{color} argument in the \code{geom_point} function.
#'     \item{\code{smooth.fill}}: a character vector with two elements (default:
#'     \code{c("#FE6100", "#56B4E9")}) indicating the \code{fill} argument in the
#'     \code{geom_smooth} function for the regression and LOESS smoothing curve
#'     \item{\code{smooth.alpha}}: a numeric vector with two elements between 0
#'     and 1 (default: \code{0.15}) indicating the \code{alpha} argument in
#'     the \code{geom_smooth} function for the regression and LOESS smoothing curve
#'     \item{\code{smooth.type}}: a numeric vector with two elements (default:
#'     \code{c("solid", "dashed")}) indicating the \code{linetype} argument in the
#'     \code{geom_smooth} function for the regression and LOESS smoothing curve.
#'     \item{\code{smooth.width}}: a numeric vector with two elements (default:
#'     \code{c(1, 0.8)}) indicating the \code{linewidth} argument in the
#'     \code{geom_smooth} function for the regression and LOESS smoothing curve.
#'     \item{\code{smooth.col}}: a character vector with two elements (default:
#'     \code{c("#FE6100", "#0057B8")}) indicating the \code{color} argument in
#'     the \code{geom_smooth} function for the regression and LOESS smoothing curve
#'     \item{\code{xlab}}: a character string (default: \code{"Predictor"}) indicating
#'     the \code{name} argument in the \code{scale_x_continuous} function.
#'     \item{\code{xlim}}: a numeric vector with two elements (default: \code{NULL})
#'     indicating the \code{limits} argument in the \code{scale_x_continuous} function.
#'     \item{\code{xbreaks}}: a numeric vector (default: \code{waiver()}) indicating
#'     the \code{breaks} argument in the \code{scale_x_continuous} function.
#'     \item{\code{ylab}}: a character string (default: \code{"Partial Residual"})
#'     indicating the \code{name} argument in the \code{scale_y_continuous} function.
#'     \item{\code{ylim}}: a numeric vector with two elements indicating the \code{limits}
#'     argument in the \code{scale_y_continuous} function.
#'     \item{\code{ybreaks}}: a numeric vector (default: \code{waiver()}) indicating
#'     the \code{breaks} argument in the \code{scale_y_continuous} function.
#'   }
#' }
#' \item{\strong{Function \code{test.norm}}}{The \code{plot} function for the
#' misty object of type \code{"test.norm"} has following plotting arguments:
#'   \itemize{
#'     \item{\code{geom}}: a character vector indicating which plot to draw, i.e.,
#'     \code{"all"} for plotting a Q-Q plot and histogram, \code{"qq"} for plotting
#'     Q-Q plots, and \code{"hist"} for plotting histograms. Note that \code{"all"}
#'     and \code{c("qq", "hist")} are valid only when specifying a fitted model
#'     of class \code{lm} or a single variable. When specifying \code{"all"} or
#'     \code{c("qq", "hist")} given multiple variables, a multivariate
#'     Mahalanobis Q-Q plot is drawn. The default setting is \code{c("qq", "hist")}.
#'     \item{\code{ribbon.fill}}: a character string (default: \code{"#56B4E9"})
#'     indicating the \code{fill} argument in the \code{geom_ribbon} function.
#'     \item{\code{ribbon.alpha}}: a numeric value between 0 and 1 (default: \code{0.1})
#'     indicating the \code{alpha} argument in the \code{geom_ribbon} function.
#'     \item{\code{ribbon.col}}: a character string (default: \code{"#0057B8"})
#'     indicating the \code{color} argument in the \code{geom_ribbon} function.
#'     \item{\code{abline.col}}: a character string (default: \code{"#0057B8"})
#'     indicating the \code{color} argument in the \code{geom_abline} function.
#'     \item{\code{abline.width}}: a numeric value (default: \code{0.7}) indicating
#'     the \code{linewidth} argument in the \code{geom_abline} function.
#'     \item{\code{point.size}}: a numeric value (default: \code{2}) indicating
#'     the \code{size} argument in the \code{geom_point} function.
#'     \item{\code{point.shape}}: a numeric value (default: \code{21}) indicating
#'     the \code{shape} argument in the \code{geom_point} function.
#'     \item{\code{point.fill}}: a character string  (default: \code{"#648FFF"})
#'     indicating the \code{fill} argument in the \code{geom_point} function.
#'     \item{\code{point.col}}: a character string  (default: \code{"black"})
#'     indicating the \code{color} argument in the \code{geom_point} function.
#'     \item{\code{hist.binwidth}}: a numeric value or a function (default: \code{NULL})
#'     indicating the \code{binwidth} argument in the \code{geom_histogram} function.
#'     \item{\code{hist.fill}}: a numeric value or a function (default: \code{"gray90"})
#'     indicating the \code{fill} argument in the \code{geom_histogram} function.
#'     \item{\code{hist.col}}: a numeric value or a function (default: \code{"black"})
#'     indicating the \code{color} argument in the \code{geom_histogram} function.
#'     \item{\code{function.type}}: a numeric or character vector of length 2 (default:
#'     \code{c("solid", "dashed")}) indicating the \code{linetype} argument in the
#'     \code{stat_function} function.
#'     \item{\code{function.width}}: a numeric vector of length 2 (default: \code{c(1, 1))})
#'     indicating the \code{linewidth} argument in the \code{stat_function} function.
#'     \item{\code{function.col}}: a character vector of length 2 (default:
#'     \code{c("#0057B8", "#D55E00")}) indicating the \code{color} argument in
#'     the \code{stat_function} function.
#'     \item{\code{xlab}}: a character string indicating the \code{name} argument
#'     in the \code{scale_x_continuous} function. The default setting is
#'     \code{"Theoretical Quantiles of the Normal Distribution"} when
#'     plotting univariate QQ plots and \code{"Chi-Square Quantile"} when plotting
#'     a multivariate QQ plot.
#'     \item{\code{xlim}}: a numeric vector with two elements indicating the
#'     \code{limits} argument in the \code{scale_x_continuous} function.
#'     \item{\code{xbreaks}}: a numeric vector (default: \code{waiver()}) indicating
#'     the \code{breaks} argument in the \code{scale_x_continuous} function.
#'     \item{\code{ylab}}: a character string indicating the \code{name} argument
#'     in the \code{scale_y_continuous} function The default setting is
#'     \code{"Sample Quantiles"} when plotting univariate QQ plots and
#'     \code{"Squared Mahalanobis Distance"} when plotting a multivariate QQ plot.
#'     \item{\code{ylim}}: a numeric vector with two elements indicating the
#'     \code{limits} argument in the \code{scale_y_continuous} function.
#'     \item{\code{ybreaks}}: a numeric vector (default: \code{waiver()}) indicating
#'     the \code{breaks} argument in the \code{scale_y_continuous} function.
#'     \item{\code{axis.title.size}}: a numeric value (default: \code{11}) indicating
#'     the \code{size} argument in the \code{element_text} function, i.e.
#'     \code{theme(axis.title = element_text(size = axis.text.size))}
#'     \item{\code{axis.text.size}}: a numeric value (default: 10) indicating the
#'     \code{size} argument in the \code{element_text} function, i.e.,
#'     \code{theme(axis.text = element_text(size = axis.text.size))}
#'     \item{\code{facet.nrow}}: a numeric value (default: \code{NULL}) indicating
#'     the \code{nrow} argument in the \code{facet_wrap} function when plotting
#'     multiple Q-Q plots or histograms.
#'     \item{\code{facet.ncol}}: a numeric value (default: \code{3}) indicating the
#'     \code{ncol} argument in the \code{facet_wrap} function when plotting multiple
#'     Q-Q plots or histograms.
#'     \item{\code{facet.scales}}: a character string indicating \code{scales}
#'     argument in the \code{facet_wrap} function. The default is \code{"free_y"}
#'     when plotting multiple Q-Q plots and \code{"free"} when plotting multiple
#'     histograms.
#'     \item{\code{strip.text.size}}: a numeric value (default: \code{10}) indicating
#'     the font size of the strip text, i.e.
#'     \code{theme(strip.text = ggplot2::element_text(size = strip.text.size))}.
#'   }
#' }
#' \item{\strong{Functions \code{test.t}, \code{test.welch}, and \code{test.z}}}
#' {The \code{plot}  function for the misty object of type \code{"test.t"},
#' \code{"test.welch"}, and \code{"test.z"} has following plotting arguments:
#' \itemize{
#'    \item{\code{geom}}: a character string or character vector indicating which
#'    plot elements to draw, i.e.,  \code{"all"} for drawing all plot elements,
#'    \code{"bar"} for drawing bars representing means for each groups, \code{"point"}
#'    for drawing points representing means for each group, \code{"ci"} for drawing
#'    error bars representing confidence intervals, \code{"jitter"} for drawing
#'    jittered data points, \code{"line"} for drawing a horizontal line at \code{mu}
#'    for the one-sample t-test or at \code{0} for the paired-sample t-test.
#'    \item{\code{conf.level}}: a numeric value between 0 and 1 (default: 0.95)
#'    indicating the confidence level of the interval.
#'    \item{\code{adjust}}: logical: if \code{TRUE} (default), difference-adjustment
#'    for the confidence intervals in a two-sample design is applied.
#'    \item{\code{point.size}}: a numeric value (default: \code{3}) indicating the
#'    \code{size} argument in the \code{geom_point} function.
#'    \item{\code{errorbar.width}}: a numeric value (default: \code{0.1}) indicating
#'    the \code{width} argument in the \code{geom_errorbar} function.
#'    \item{\code{hline.type}}: an integer value or character string (default: \code{3})
#'    indicating the \code{linetype} argument in the \code{geom_hline} function.
#'    \item{\code{hline.width}}: a numeric value (default: \code{0.8}) indicating
#'    the \code{linewidth} argument in the \code{geom_hline} function.
#'    \item{\code{jitter.size}}: a numeric value (default: \code{1.25}) indicating
#'    the \code{size} argument in the \code{geom_jitter} function.
#'    \item{\code{jitter.width}}: a numeric value (default: \code{0.05}) indicating
#'    the \code{width} argument in the \code{geom_jitter} function.
#'    \item{\code{jitter.height}}: a numeric value (default: \code{0}) indicating
#'    the \code{height} argument in the \code{geom_jitter} function.
#'    \item{\code{jitter.alpha}}: a numeric value between 0 and 1 (default: \code{0.1})
#'    the \code{alpha} argument in the \code{geom_jitter} function.
#'    \item{\code{xlab}}: a character string (default: \code{NULL}) indicating
#'    the \code{name} argument in the \code{scale_x_continuous} function.
#'    \item{\code{ylab}}: a character string (default: \code{"y"}) indicating
#'    the \code{name} argument in the \code{scale_y_continuous} function.
#'    \item{\code{ylim}}: a numeric vector with two elements (default: \code{NULL})
#'    indicating the \code{limits} argument in the \code{scale_y_continuous} function.
#'    \item{\code{ybreaks}}: a numeric vector (default: \code{waiver()}) indicating
#'    the \code{breaks} argument in the \code{scale_y_continuous} function.
#'    \item{\code{title}}: a character string (default: \code{""}) indicating the
#'    \code{title} argument in the \code{labs} function.
#'    \item{\code{subtitle}}: a character string (default: \code{""}) indicating the
#'    \code{subtitle} argument in the \code{labs} function The default setting is
#'    \code{"Two-Sided Confidence Interval"} when \code{adjust = FALSE} or
#'    \code{"Two-Sided Difference-Adjusted Confidence Interval"}
#'    when \code{adjust = TRUE}.
#'    \item{\code{facet_ncol}}: a numeric value (default: \code{3}) indicating the
#'    \code{ncol} argument in the \code{facet_wrap} function when conducting
#'    multiple two-sample t-tests or Welch tests.
#'    \item{\code{facet_nrow}}: a numeric value (default: \code{NULL}) indicating
#'    the \code{nrow} argument in the \code{facet_wrap} function when conducting
#'    multiple two-sample t-tests or Welch tests.
#'    \item{\code{facet.scales}}: a character string (default: \code{"free_y"})
#'    indicating the \code{scales} argument in the \code{facet_wrap} function when
#'    conducting multiple two-sample t-tests or Welch tests.
#' }
#' }
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @method plot misty.object
#'
#' @export
plot.misty.object <- function(x, plot = NULL, geom = NULL, color = NULL, type = NULL,
                              line.type = NULL, line.width = NULL, line.col = NULL,
                              ci.col = NULL, ci.width = NULL, ci.type = NULL,
                              errorbar.width = NULL, errorbar.dodge.width = NULL,
                              smooth.fill = NULL, smooth.alpha = NULL, smooth.type = NULL, smooth.width = NULL, smooth.col = NULL,
                              point.size = NULL, point.width = NULL, point.type = NULL, point.shape = NULL, point.col = NULL, point.fill = NULL,
                              jitter.size = NULL, jitter.width = NULL, jitter.height = NULL, jitter.alpha = NULL,
                              ribbon.fill = NULL, ribbon.col = NULL, ribbon.alpha = NULL,
                              violin.alpha = NULL, violin.trim = NULL,
                              density.col = NULL, density.width = NULL, density.type = NULL, density.alpha = NULL,
                              hist.binwidth = NULL, hist.bins = NULL, hist.col = NULL, hist.fill = NULL, hist.alpha = NULL,
                              box.width = NULL, box.alpha = NULL,
                              tile.col = NULL, tile.alpha = NULL,
                              function.type = NULL, function.width = NULL, function.col = NULL,
                              abline.col = NULL, abline.width = NULL,
                              hline.intercept = NULL, hline.type = NULL, hline.width = NULL, hline.col = NULL,
                              vline.intercept = NULL, vline.type = NULL, vline.width = NULL, vline.col = NULL,
                              text.size = NULL, text.col = NULL, text.font = NULL, gray = NULL, start = NULL, end = NULL,
                              xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, xbreaks = NULL, ybreaks = NULL,
                              axis.title.size = NULL, axis.text.size = NULL, strip.text.size = NULL,
                              scale.fill.col = NULL, scale.fill.low = NULL, scale.fill.high = NULL, scale.fill.mid = NULL,
                              title = NULL, subtitle = NULL, group.col = NULL, plot.margin = NULL,
                              legend.title = NULL, legend.position = NULL, legend.box.margin = NULL, legend.key.size = NULL, legend.text.size = NULL,
                              facet.ncol = NULL, facet.nrow = NULL, facet.scales = NULL,
                              conf.level = NULL, adjust = NULL,
                              filename = NULL, width = NULL, height = NULL, units = NULL, dpi = NULL, check = TRUE, ...) {

  #_____________________________________________________________________________
  #
  # Initial Check ——————————————————————————————————————————————————————————————

  # Check if input 'x' is missing or NULL or not a misty object
  if (isTRUE(missing(x) || is.null(x) || !inherits(x, what = "misty.object"))) { stop("Please specify a misty object for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is supported by the function
  if (isTRUE(!x$type %in% c("aov.b", "aov.w", "boot.bs", "check.outlier", "check.resid", "ci.cor", "ci.mean", "ci.median", "ci.prop", "ci.var", "ci.sd", "cor.matrix", "item.dfi", "multilevel.cor", "multilevel.r2", "multilevel.r2.manual", "na.pattern", "test.homo", "test.levene", "test.linear", "test.norm", "test.welch", "test.t", "test.z"))) { stop("This type of misty object is not supported by the plot.misty.object() function.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Global Variables  ——————————————————————————————————————————————————————————

  cook <- hat <- idout <-  mahal <- rstand <- rstud <- chisq <- group <- id <- low <- m <- m.diff <- m.low <- m.upp <- mu <- n.pattern <- obs_miss <- part <- pattern <- upp <- variable <- lower <- upper <- slope <- pred <- cor.format <- intercept <- sig <- NULL

  #_____________________________________________________________________________
  #
  # Global Default Argument Specification  —————————————————————————————————————

  .plot.argument(plot = ".x", conf.level = ".x", adjust = ".x", axis.title.size = 11L, axis.text.size = 10L, strip.text.size = 11L, filename = ".x", width = ".x", height = ".x", units = "in", dpi = ".x", x = x, envir = environment())

  #_____________________________________________________________________________
  #
  # Between-Subject Analysis of Variance (ANOVA) ———————————————————————————————

  switch(x$type, aov.b = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    .plot.argument(geom = c("bar", "point", "ci", "line", "jitter"), point.size = 3L, errorbar.width = 0.1, jitter.size = 1.25, jitter.width = 0.05, jitter.height = 0L, jitter.alpha = 0.1, subtitle = "Confidence Interval", ylab = "y", ybreaks = "ggplot2::waiver()", x = x, envir = environment())

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Means and (Difference-Adjusted) Confidence Intervals by Group ####

    ci.table <- misty::ci.mean(x$data, y, group = "group", adjust = adjust, conf.level = conf.level, output = FALSE)$result

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Create ggplot ####

    # Subtitle
    if (isTRUE("ci" %in% geom)) { if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% ", ifelse(isTRUE(adjust), "Difference-Adjusted ", ""), "Confidence Interval") } else { subtitle } } else { subtitle <- "" }

    p <- ggplot2::ggplot(x$data, ggplot2::aes(group, y)) +
      ggplot2::scale_x_discrete(name = xlab) +
      ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = title, subtitle = subtitle) +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5), plot.title = ggplot2::element_text(hjust = 0.5))

    #—————————————————————————————————————— #
    ### Bars ####

    if (isTRUE("bar" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_bar(data = ci.table, ggplot2::aes(group, m), stat = "summary", fun = "mean")) }

    #—————————————————————————————————————— #
    ### Jittered Points ####

    if (isTRUE("jitter" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_jitter(alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

    #—————————————————————————————————————— #
    ### Confidence Intervals ####

    if (isTRUE("ci" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(data = ci.table, ggplot2::aes(group, m, ymin = low, ymax = upp), width = errorbar.width)) }

    #—————————————————————————————————————— #
    ### Points ####

    if (isTRUE("point" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_point(data = ci.table, ggplot2::aes(group, m), stat = "identity", size = point.size)) }

  #_____________________________________________________________________________
  #
  # Repeated Measures Analysis of Variance (Within-Subject ANOVA ———————————————

  }, aov.w = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    .plot.argument(geom = c("point", "line", "ci", "jitter"), line.type = "solid", line.width = 0.5, point.size = 3L, errorbar.width = 0.1, jitter.size = 1.25, jitter.width = 0.05, jitter.height = 0L, jitter.alpha = 0.1, subtitle = "Confidence Interval", ylab = "y", ybreaks = "ggplot2::waiver()", x = x, envir = environment())

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Compute Means and (Difference-Adjusted) Confidence Intervals by Group ####

    ci.table <- misty::ci.mean.w(x$data$wide, adjust = adjust, conf.level = conf.level, na.omit = x$args$na.omit, check = FALSE, output = FALSE)$result

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Create ggplot ####

    # Subtitle
    if (isTRUE("ci" %in% geom)) { if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% ", ifelse(isTRUE(adjust), "Difference-Adjusted ", ""), "Cousineau-Morey Confidence Interval") } else { subtitle } } else { subtitle <- "" }

    p <- ggplot2::ggplot(x$data$long, ggplot2::aes(time, y, group = 1L)) +
      ggplot2::scale_x_discrete(name = xlab) +
      ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = title, subtitle = subtitle) +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5), plot.title = ggplot2::element_text(hjust = 0.5))

    #—————————————————————————————————————— # #
    ### Jittered Points ####

    if (isTRUE("jitter" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_jitter(alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

    #—————————————————————————————————————— # #
    ### Individual Lines ####

    if (isTRUE("jitter" %in% geom && "line" %in% geom)) { p <- p + ggplot2::geom_line(data = x$data$long, ggplot2::aes(time, y, group = id), alpha = jitter.alpha, position = ggplot2::position_dodge(jitter.width)) }

    #—————————————————————————————————————— #
    ### Confidence Intervals ####

    if (isTRUE("ci" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(data = ci.table, ggplot2::aes(variable, m, ymin = low, ymax = upp), width = errorbar.width)) }

    #—————————————————————————————————————— #
    ### Points ####

    if (isTRUE("point" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_point(data = ci.table, ggplot2::aes(variable, m), stat = "identity", size = point.size)) }

    #—————————————————————————————————————— # #
    ### Lines ####

    if (isTRUE("line" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_line(data = ci.table, ggplot2::aes(variable, m), stat = "identity", linewidth = line.width, linetype = line.type)) }

  #_____________________________________________________________________________
  #
  # Bollen-Stine Bootstrap with Incomplete Data, boot.bs() —————————————————————

  }, boot.bs = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Plot Data ####

    plotdat <- data.frame(chisq = x$boot.chisq)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Create ggplot ####

    p <- ggplot2::ggplot(plotdat, ggplot2::aes(chisq)) +
      ggplot2::theme_bw() +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), color = "black", alpha = 0.4, fill = "gray85") +
      ggplot2::geom_density(color = "#0072B2") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = x$result$chisq, color = "Observed Test Statistic")) +
      ggplot2::scale_x_continuous(name = expression(paste(chi^2, " Test Statistic")), limits = c(0L, max(c(plotdat$chisq, x$result$chisq), na.rm = TRUE))) +
      ggplot2::scale_y_continuous(name = "Probability Density, f(x)", expand = ggplot2::expansion(mult = c(0L, 0.05))) +
      ggplot2::scale_color_manual(values = c("Observed Test Statistic" = "#CC79A7")) +
      ggplot2::theme(legend.position = "bottom",
                     legend.box.margin = ggplot2::margin(-15L, 0L, 0L, 0L),
                     legend.title = ggplot2::element_blank(),
                     legend.background = ggplot2::element_rect(fill = "transparent"))

  #_____________________________________________________________________________
  #
  # Outlier and Influential Observations Diagnostics for Linear Models —————————

  }, check.outlier = {

    plot.check.outlier <- paste0("p.", misty::chr.omit(names(x$data), omit = "data", check = FALSE))

    invisible(sapply(plot.check.outlier, function(y) { eval(parse(text = paste0(y, " <<- NULL"))) }))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    .plot.argument(type = ".x", geom = ".x", point.shape = 21L, point.size = 2L, point.fill = "#648FFF", point.col = "black",
                   line.type = "solid", line.width = 0.5, line.col = "#648FFF",
                   hline.type = "dashed", hline.width = 0.5, hline.col = "#FE6100",
                   text.size = 3L, text.col = "black", ybreaks = "ggplot2::waiver()", x = x, envir = environment())

    #—————————————————————————————————————— #
    ### Mahalanobis Distance ####

    if (isTRUE("mahal" %in% type)) {

      # idout
      x$data$mahal$idout <- factor(x$data$mahal$idout, levels = x$data$mahal$idout)

      # Create ggplot
      p.mahal <- ggplot2::ggplot(x$data$mahal, ggplot2::aes(x = idout, y = mahal, label = idout, ymin = 0, ymax = mahal)) +
        ggplot2::geom_linerange(linetype = line.type, linewidth = line.width, color = line.col) +
        ggplot2::geom_point(size = point.size, shape = point.shape, fill = point.fill, color = point.col) +
        ggplot2::theme_bw() +
        ggplot2::scale_x_discrete("") +
        ggplot2::scale_y_continuous("Mahalanobis Distance", limits = ylim, breaks = ybreaks, expand = ggplot2::expansion(mult = c(0, ifelse("text" %in% geom, 0.1, 0.05)))) +
        ggplot2::theme(axis.title = ggplot2::element_text(size = axis.title.size),
                       axis.text = ggplot2::element_text(size = axis.text.size))

      # Add text
      if (isTRUE("text" %in% geom)) { p.mahal <- p.mahal + ggplot2::geom_text(vjust = -1, size = text.size, color = text.col) }

      # Add cutoff
      if (isTRUE(!is.null(x$args$cutoff) && "mahal" %in% names(x$args$cutoff))) { p.mahal <- p.mahal + ggplot2::geom_hline(yintercept = x$args$cutoff["mahal"], linewidth = hline.width, linetype = hline.type, color = hline.col) }

    }

    #—————————————————————————————————————— #
    ### Hat Values ####

    if (isTRUE("hat" %in% type)) {

      # idout
      x$data$hat$idout <- factor(x$data$hat$idout, levels = x$data$hat$idout)

      # Create ggplot
      p.hat <- ggplot2::ggplot(x$data$hat, ggplot2::aes(x = idout, y = hat, label = idout, ymin = 0, ymax = hat)) +
        ggplot2::geom_linerange(linetype = line.type, linewidth = line.width, color = line.col) +
        ggplot2::geom_point(size = point.size, shape = point.shape, fill = point.fill, color = point.col) +
        ggplot2::theme_bw() +
        ggplot2::scale_x_discrete("") +
        ggplot2::scale_y_continuous("Hat Value", limits = ylim, breaks = ybreaks, expand = ggplot2::expansion(mult = c(0, ifelse("text" %in% geom, 0.1, 0.05)))) +
        ggplot2::theme(axis.title = ggplot2::element_text(size = axis.title.size),
                       axis.text = ggplot2::element_text(size = axis.text.size))

      # Add text
      if (isTRUE("text" %in% geom)) { p.hat <- p.hat + ggplot2::geom_text(vjust = -1, size = text.size, color = text.col) }

      # Add cutoff
      if (isTRUE(!is.null(x$args$cutoff) && "hat" %in% names(x$args$cutoff))) { p.hat <- p.hat + ggplot2::geom_hline(yintercept = x$args$cutoff["hat"], linewidth = hline.width, linetype = hline.type, color = hline.col) }

    }

    #—————————————————————————————————————— #
    ### Standardized Leverage-Corrected Residuals ####

    if (isTRUE("rstand" %in% type)) {

      # idout
      x$data$rstand$idout <- factor(x$data$rstand$idout, levels = x$data$rstand$idout)

      # Create ggplot
      p.rstand <- ggplot2::ggplot(x$data$rstand, ggplot2::aes(x = idout, y = rstand, label = idout, ymin = 0, ymax = rstand)) +
        ggplot2::geom_linerange(linetype = line.type, linewidth = line.width, color = line.col) +
        ggplot2::geom_point(size = point.size, shape = point.shape, fill = point.fill, color = point.col) +
        ggplot2::geom_hline(yintercept = 0L,linetype = line.type, linewidth = line.width, color = line.col) +
        ggplot2::theme_bw() +
        ggplot2::scale_x_discrete("") +
        ggplot2::scale_y_continuous("Standardized Leverage-Corrected Residual", limits = ylim, breaks = ybreaks, expand = ggplot2::expansion(mult = c(ifelse("text" %in% geom, 0.1, 0.05), ifelse("text" %in% geom, 0.1, 0.05)))) +
        ggplot2::theme(axis.title = ggplot2::element_text(size = axis.title.size),
                       axis.text = ggplot2::element_text(size = axis.text.size))

      # Add text
      if (isTRUE("text" %in% geom)) { p.rstand <- p.rstand + ggplot2::geom_text(ggplot2::aes(vjust = ifelse(rstand > 0, -1, 1.5)), size = text.size, color = text.col) }

      # Add cutoff
      if (isTRUE(!is.null(x$args$cutoff) && "rstand" %in% names(x$args$cutoff))) {

        p.rstand <- p.rstand + ggplot2::geom_hline(yintercept = x$args$cutoff["rstand"], linewidth = hline.width, linetype = hline.type, color = hline.col) +
                               ggplot2::geom_hline(yintercept = -x$args$cutoff["rstand"], linewidth = hline.width, linetype = hline.type, color = hline.col)

      }

    }

    #—————————————————————————————————————— #
    ### Studentized Leverage-Corrected Residuals ####

    if (isTRUE("rstud" %in% type)) {

      # idout
      x$data$rstud$idout <- factor(x$data$rstud$idout, levels = x$data$rstud$idout)

      # Create ggplot
      p.rstud <- ggplot2::ggplot(x$data$rstud, ggplot2::aes(x = idout, y = rstud, label = idout, ymin = 0, ymax = rstud)) +
        ggplot2::geom_linerange(color = line.col) +
        ggplot2::geom_point(size = point.size, shape = point.shape, fill = point.fill, color = point.col) +
        ggplot2::geom_hline(yintercept = 0L, color = line.col) +
        ggplot2::theme_bw() +
        ggplot2::scale_x_discrete("") +
        ggplot2::scale_y_continuous("Studentized Leverage-Corrected Residual", limits = ylim, breaks = ybreaks, expand = ggplot2::expansion(mult = c(ifelse("text" %in% geom, 0.1, 0.05), ifelse("text" %in% geom, 0.1, 0.05)))) +
        ggplot2::theme(axis.title = ggplot2::element_text(size = axis.title.size),
                       axis.text = ggplot2::element_text(size = axis.text.size))

      # Add text
      if (isTRUE("text" %in% geom)) { p.rstud <- p.rstud + ggplot2::geom_text(ggplot2::aes(vjust = ifelse(rstud > 0, -1, 1.5)), size = text.size, color = text.col) }

      # Add cutoff
      if (isTRUE(!is.null(x$args$cutoff) && "rstud" %in% names(x$args$cutoff))) {

        p.rstud <- p.rstud + ggplot2::geom_hline(yintercept = x$args$cutoff["rstud"], linewidth = hline.width, linetype = hline.type, color = hline.col) +
                               ggplot2::geom_hline(yintercept = -x$args$cutoff["rstud"], linewidth = hline.width, linetype = hline.type, color = hline.col)

      }
    }

    #—————————————————————————————————————— #
    ### Cookꞌs Distance ####

    if (isTRUE("cook" %in% type)) {

      # idout
      x$data$cook$idout <- factor(x$data$cook$idout, levels = x$data$cook$idout)

      # Create ggplot
      p.cook <- ggplot2::ggplot(x$data$cook, ggplot2::aes(x = idout, y = cook, label = idout, ymin = 0, ymax = cook)) +
        ggplot2::geom_linerange(linetype = line.type, linewidth = line.width, color = line.col) +
        ggplot2::geom_point(size = point.size, shape = point.shape, fill = point.fill, color = point.col) +
        ggplot2::theme_bw() +
        ggplot2::scale_x_discrete("") +
        ggplot2::scale_y_continuous("Cook's Distance", limits = ylim, breaks = ybreaks, , expand = ggplot2::expansion(mult = c(0L, ifelse("text" %in% geom, 0.1, 0.05)))) +
        ggplot2::theme(axis.title = ggplot2::element_text(size = axis.title.size),
                       axis.text = ggplot2::element_text(size = axis.text.size))

      # Add text
      if (isTRUE("text" %in% geom)) { p.cook <- p.cook + ggplot2::geom_text(vjust = -1, size = text.size, color = text.col) }

      # Add cutoff
      if (isTRUE(!is.null(x$args$cutoff) && "cook" %in% names(x$args$cutoff))) { p.cook <- p.cook + ggplot2::geom_hline(yintercept = x$args$cutoff["cook"], linewidth = hline.width, linetype = hline.type, color = hline.col) }

    }

    #—————————————————————————————————————— #
    ### DFBETAS ####

    if (isTRUE("dfb" %in% type)) {

      for (i in misty::chr.omit(plot.check.outlier, omit = c("p.mahal", "p.hat", "p.rstand", "p.rstud", "p.cook"))) {

        dfb <- sub("p.", "", i)

        # idout
        eval(parse(text = paste0("x$data$", dfb, "$idout <- factor(x$data$", dfb, "$idout, levels = x$data$", dfb, "$idout)")))

        # Create ggplot
        eval(parse(text = paste0(i, " <- ggplot2::ggplot(x$data$", dfb, ", ggplot2::aes(x = idout, y = ", dfb, ", label = idout, ymin = 0, ymax = ", dfb, ")) +
          ggplot2::geom_linerange(color = line.col) +
          ggplot2::geom_point(size = point.size, shape = point.shape, fill = point.fill, color = point.col) +
          ggplot2::geom_hline(yintercept = 0L, color = line.col) +
          ggplot2::theme_bw() +
          ggplot2::scale_x_discrete(\"\") +
          ggplot2::scale_y_continuous(", expression(paste0("DFBETAS: ", sub(".dfb", "", dfb))), ", limits = ylim, breaks = ybreaks, expand = ggplot2::expansion(mult = c(ifelse(\"text\" %in% geom, 0.1, 0.05), ifelse(\"text\" %in% geom, 0.1, 0.05)))) +
          ggplot2::theme(axis.title = ggplot2::element_text(size = axis.title.size),
                         axis.text = ggplot2::element_text(size = axis.text.size))")))

        # Add text
        eval(parse(text = paste0("if (isTRUE(\"text\" %in% geom)) { ", i, " <- ", i,  " + ggplot2::geom_text(ggplot2::aes(vjust = ifelse(", dfb, " > 0, -1, 1.5)), size = text.size, color = text.col) }")))


        # Add cutoff
        eval(parse(text = paste0("if (isTRUE(!is.null(x$args$cutoff) && \"dfb\" %in% names(x$args$cutoff))) {

              ", i, " <- ", i, " + ggplot2::geom_hline(yintercept = x$args$cutoff[\"dfb\"], linewidth = hline.width, linetype = hline.type, color = hline.col) +
                                   ggplot2::geom_hline(yintercept = -x$args$cutoff[\"dfb\"], linewidth = hline.width, linetype = hline.type, color = hline.col)

           }")))

        }

    }


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Arrange Plots ####

    p <- eval(parse(text = paste0("patchwork::wrap_plots(", paste0(misty::chr.omit(plot.check.outlier, omit = names(which(sapply(plot.check.outlier, function(y) is.null(eval(parse(text = y))))))),  collapse = ", "), ", ncol = 1L)")))

  #_____________________________________________________________________________
  #
  # Residual Diagnostics for Linear, Multilevel and Mixed-Effects Models ———————

  }, check.resid = {

    pred <- condval <- NULL

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Linearity ####

    switch(x$args$type, linear = {

      #—————————————————————————————————————— #
      ### Default Argument Specification ####

      .plot.argument(geom = c("lm", "loess", "ci.lm", "ci.loess", "qq", "hist"), point.shape = 21L, point.size = 2L, point.fill = "#648FFF",
                     point.col = "black", smooth.fill = c("#FE6100", "#56B4E9"), smooth.alpha = c(0.1, 0.15), smooth.type = c("solid", "dashed"),
                     smooth.width = c(1L, 0.8), smooth.col = c("#FE6100", "#0057B8"), xlab = "Predictor", xbreaks = "ggplot2::waiver()",
                     ylab = "Partial Residual", ybreaks = "ggplot2::waiver()", x = x, envir = environment())

      #—————————————————————————————————————— #
      ### Linear Model ####

      if (isTRUE(class(x$model) == "lm")) {

        #···················
        #### Predictor Variables ####

        pred.names <- attr(x$model$terms, "term.labels")

        #···················
        #### Data Frame ####

        plotdat <- data.frame(var = factor(rep(pred.names, each = nrow(x$model$model)), levels = pred.names),
                              pred = unlist(x$model$model[, pred.names]),
                              resid = as.vector(residuals(x$model, type = "partial")))

      #—————————————————————————————————————— #
      ### Multilevel and Linear Mixed-Effects Model, lmer() ####

      } else if (isTRUE(class(x$model) %in% c("lmerMod", "lmerModLmerTest"))) {

        #···················
        #### Data Frame ####

        # Model Data
        model.data <- model.frame(x$model)

        # Names of the predictors
        pred.names <- names(lme4::fixef(x$model) |> (\(y) y[names(y) != "(Intercept)"])())

        # Data
        plotdat <- data.frame(var = factor(rep(pred.names, each = nrow(model.data)), levels = pred.names),
                              pred = unlist(model.data[, pred.names]),
                              resid = as.vector(sapply(pred.names, function(y) .resid.partial(x$model, fix = if (length(pred.names) != 1L) { misty::chr.omit(pred.names, omit = y) } else { NULL }, ran = "all"))))

      }

      #—————————————————————————————————————— #
      ### Create ggplot ####

      p <- ggplot2::ggplot(plotdat, ggplot2::aes(pred, resid))

      # Add regression line
      if (isTRUE("lm" %in% geom)) { p <- p + ggplot2::geom_smooth(method = "lm", se = ifelse(isTRUE("ci.lm" %in% geom), TRUE, FALSE), formula = "y ~ x", linetype = smooth.type[1L], linewidth = smooth.width[1L], color = smooth.col[1L], fill = smooth.fill[1L], alpha = smooth.alpha[1L]) }

      # Add less line
      if (isTRUE("loess" %in% geom)) { p <- p + ggplot2::geom_smooth(method = "loess", se = ifelse(isTRUE("ci.loess" %in% geom), TRUE, FALSE), formula = "y ~ x", linetype =  smooth.type[2L], linewidth = smooth.width[2L], color = smooth.col[2L], fill = smooth.fill[2L], alpha = smooth.alpha[2L]) }

      p <- p + ggplot2::geom_point(size = point.size, shape = point.shape, fill = point.fill, color = point.col) +
        ggplot2::facet_wrap(~ var, scales = "free") +
        ggplot2::theme_bw() +
        ggplot2::scale_x_continuous("Predictor", limits = xlim, breaks = xbreaks) +
        ggplot2::scale_y_continuous("Partial Residual", limits = ylim, breaks = ybreaks) +
        ggplot2::theme(strip.text = ggplot2::element_text(size = strip.text.size),
                       axis.title = ggplot2::element_text(size = axis.title.size),
                       axis.text = ggplot2::element_text(size = axis.text.size))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Homoscedasticity ####

    }, homo = {

      #—————————————————————————————————————— #
      ### Default Argument Specification ####

      .plot.argument(geom = c("hline", "loess", "ci.loess"), hline.type = "solid", hline.width = 1L, hline.col = "#FE6100", point.shape = 21L, point.size = 2L, point.fill =  "#648FFF", point.col = "black", smooth.fill = "#56B4E9" , smooth.alpha = 0.15, smooth.type = "dashed", smooth.width = 0.8, smooth.col = "#0057B8", xbreaks = "ggplot2::waiver()", ybreaks = "ggplot2::waiver()", x = x, envir = environment())

      #—————————————————————————————————————— #
      ### Linear Model ####

      if (isTRUE(class(x$model) == "lm")) {

        #···················
        #### Residual ####

        # Unstandardized residuals
        switch(x$args$resid, unstand = {

          plotdat <- data.frame(pred = predict(x$model), resid = residuals(x$model))

        # Standardized residuals
        }, stand =  {

          plotdat <- data.frame(pred = predict(x$model), resid = rstandard(x$model))

        # Studentized residuals
        }, student = {

          plotdat <- data.frame(pred = predict(x$model), resid = rstudent(x$model))

        })

        #···················
        #### Limits and of the x and y-axis ####

        if (isTRUE(is.null(ylim))) { ylim <- c(-ceiling(max(abs(plotdat$resid))), ceiling(max(abs(plotdat$resid)))) }

      #—————————————————————————————————————— #
      ### Multilevel and Linear Mixed-Effects Model, lmer() ####

      } else if (isTRUE(class(x$model) %in% c("lmerMod", "lmerModLmerTest"))) {

        #···················
        #### Residuals ####

        # Unstandardized residuals
        switch(x$args$resid, unstand = {

          plotdat <- data.frame(pred = predict(x$model), resid = residuals(x$model))

          # Standardized residuals
        }, stand =  {

          plotdat <- data.frame(pred = predict(x$model), resid = resid(x$model, type = "pearson", scale = TRUE))

          # Studentized residuals
        }, student = {

          plotdat <- data.frame(pred = predict(x$model), resid = rstudent(x$model))

        })

        #···················
        #### Limits and of the x and y-axis ####

        if (isTRUE(is.null(ylim))) { ylim <- c(-ceiling(max(abs(plotdat$resid))), ceiling(max(abs(plotdat$resid)))) }

      }

      #—————————————————————————————————————— #
      ### Create ggplot ####

      p <- ggplot2::ggplot(plotdat, ggplot2::aes(pred, resid))

      # Add horizontal line
      if (isTRUE("hline" %in% geom)) { p <- p + ggplot2::geom_hline(yintercept = 0L, linetype = hline.type, linewidth = hline.width, color = hline.col) }

      # Add smoothing curve
      if (isTRUE("loess" %in% geom)) { p <- p + ggplot2::geom_smooth(method = loess, fill = smooth.fill, alpha = smooth.alpha, se = ifelse(isTRUE("ci.loess" %in% geom), TRUE, FALSE), formula = "y ~ x", linetype = smooth.type, linewidth = smooth.width, color = smooth.col) }

      p <- p + ggplot2::geom_point(size = point.size, shape = point.shape, fill = point.fill, color = point.col) +
        ggplot2::theme_bw() +
        ggplot2::scale_x_continuous(name = "Predicted Value", limits = xlim, breaks = xbreaks) +
        ggplot2::scale_y_continuous(name = "Studentized Residual", limits = ylim, breaks = ybreaks) +
        ggplot2::theme(axis.title = ggplot2::element_text(size = axis.title.size),
                       axis.text = ggplot2::element_text(size = axis.text.size))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Normality of Residuals ####

    }, normal =  {

      #—————————————————————————————————————— #
      ### Default Argument Specification ####

      .plot.argument(geom = c("qq", "hist"), hist.col = "black", hist.fill = "gray90", abline.col = "#0057B8", abline.width = 0.7, point.shape = 21L, point.size = 2L, point.fill = "#648FFF", point.col = "black", function.type = c("solid", "dashed"), function.width = c(1L, 1L), function.col = c("#0057B8", "#D55E00"),
                     xbreaks = "ggplot2::waiver()", ybreaks = "ggplot2::waiver()", x = x, envir = environment())

      #—————————————————————————————————————— #
      ### Linear Model ####

      if (isTRUE(class(x$model) == "lm")) {

        #···················
        #### Residuals ####

        # Unstandardized residuals
        switch(x$args$resid, unstand = {

          plotdat <- data.frame(resid = residuals(x$model))

        # Standardized residuals
        }, stand =  {

          plotdat <- data.frame(resid = rstandard(x$model))

        # Studentized residuals
        }, student = {

          plotdat <- data.frame(resid = rstudent(x$model))

        })

        #···················
        #### Q-Q Plot ####

        p1 <- ggplot2::ggplot(plotdat, ggplot2::aes(sample = resid)) +
          ggplot2::stat_qq(size = point.size, shape = point.shape, fill = point.fill) +
          ggplot2::theme_bw() +
          ggplot2::scale_x_continuous(if (isTRUE(x$args$resid == "student")) { "Theoretical Quantiles of the t Distribution"  } else { "Theoretical Quantiles of the Normal Distribution" }, limits = xlim, breaks = xbreaks) +
          ggplot2::scale_y_continuous(switch(x$args$resid, unstand = "Sample Quantile of Unstandardized Residual", stand = "Sample Quantile of Standardized Residual", student = "Sample Quantile of Studentized Residual"), limits = ylim, breaks = ybreaks) +
          ggplot2::theme(strip.text = ggplot2::element_text(size = strip.text.size),
                         axis.title = ggplot2::element_text(size = axis.title.size),
                         axis.text = ggplot2::element_text(size = axis.text.size))

        # Add line
        if (isTRUE(x$args$resid == "student")) {

          p1 <- p1 + ggplot2::stat_qq_line(distribution = qt, dparams = length(plotdat$resid) - length(attr(x$model$terms, "term.labels")) - 2L, color = abline.col)

        } else {

          p1 <- p1 + ggplot2::stat_qq_line(color = abline.col)

        }

        #···················
        #### Histogram ####

        # Limits of the x-axis
        if (isTRUE(is.null(xlim))) { xlim <- c(-ceiling(max(abs(plotdat$resid))), ceiling(max(abs(plotdat$resid)))) }

        # Plot data
        p2 <- ggplot2::ggplot(plotdat, ggplot2::aes(resid)) +
          ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), binwidth = hist.binwidth, bins = hist.bins, color = hist.col, fill = hist.fill) +
          ggplot2::stat_function(fun = if (isTRUE(x$args$resid == "student")) { dt } else { dnorm },
                                 args = if (isTRUE(x$args$resid == "student")) { list(df = length(na.omit(plotdat$resid)) - length(attr(x$model$terms, "term.labels")) - 2L) } else { list(mean = mean(plotdat$resid, na.rm = TRUE), sd = sd(plotdat$resid, na.rm = TRUE)) }, col = function.col[1L], linetype = function.type[1L], linewidth = function.width[1L]) +
          ggplot2::geom_density(color = function.col[2L], linetype = function.type[2L], linewidth = function.width[2L]) +
          ggplot2::scale_x_continuous(switch(x$args$resid, unstand = "Unstandardized Residual", stand = "Standandardized Residual", student = "Studentized Residual"), limits = xlim, breaks = xbreaks) +
          ggplot2::theme_bw() +
          ggplot2::scale_y_continuous("Density", limits = ylim, breaks = ybreaks) +
          ggplot2::theme(strip.text = ggplot2::element_text(size = strip.text.size),
                         axis.title = ggplot2::element_text(size = axis.title.size),
                         axis.text = ggplot2::element_text(size = axis.text.size))

        #···················
        #### Arrange Plots ####

        #···················
        #### Arrange Plots ####

        if (isTRUE(all(c("qq", "hist") %in% geom))) {

          p <- patchwork::wrap_plots(p1, p2, ncol = 2L)

        } else if (isTRUE(all("qq" %in% geom))) {

          p <- p1

        } else if (isTRUE(all("hist" %in% geom))) {

          p <- p2

        }

      #—————————————————————————————————————— #
      ### Multilevel and Linear Mixed-Effects Model, lmer() ####

      } else if (isTRUE(class(x$model) %in% c("lmerMod", "lmerModLmerTest"))) {

        #···················
        #### Level-1 Residuals ####

        if (isTRUE(x$args$resid == "student")) { stop("Studentized residuals are not available for multilevel and linear mixed-effects model.", call. = FALSE) }

        # Unstandardized residuals
        switch(x$args$resid, unstand = {

          plotdat1 <- data.frame(resid = residuals(x$model))

        # Standardized residuals
        }, stand =  {

          plotdat1 <- data.frame(resid = resid(x$model, type = "pearson", scale = TRUE))

        })

        #···················
        #### Level-2 Residuals ####

        # Unstandardized or standardized residuals
        plotdat2 <- as.data.frame(lme4::ranef(x$model)) |>
          (\(y) merge(y, sqrt(diag(VarCorr(x$model)[[1L]])) |> (\(z) data.frame(term = names(z), sd = z, row.names = NULL))(), by = "term"))() |>
          (\(w) if (isTRUE(x$args$resid == "stand")) { within(w, assign("condval", condval / sd)) } else { w })()

        # Level-1 and Level-2 Residuals
        plotdat <- list(plotdat1, plotdat2)

        #···················
        #### Level-1 Q-Q Plot ####

        p11 <- ggplot2::ggplot(plotdat1, ggplot2::aes(sample = resid)) +
          ggplot2::stat_qq(size = point.size, shape = point.shape, fill = point.fill) +
          ggplot2::stat_qq_line(color = abline.col) +
          ggplot2::scale_x_continuous("Theoretical Quantiles of the Normal Distribution", limits = xlim, breaks = xbreaks) +
          ggplot2::scale_y_continuous(switch(x$args$resid, unstand = "Sample Quantile of Unstandardized Residual", stand = "Sample Quantile of Standardized Residual"), limits = ylim, breaks = ybreaks) +
          ggplot2::theme_bw() +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = -0.05, vjust = 2.5),
                         strip.text = ggplot2::element_text(size = strip.text.size),
                         axis.title = ggplot2::element_text(size = axis.title.size),
                         axis.text = ggplot2::element_text(size = axis.text.size),
                         plot.margin = ggplot2::margin(5.5, 5.5, 20, 5.5, "pt")) +
          ggplot2::ggtitle("Level-1 Residuals")

        #···················
        #### Level-2 Q-Q Plot ####

        p21 <- ggplot2::ggplot(plotdat2, ggplot2::aes(sample = condval)) +
          ggplot2::stat_qq(size = point.size, shape = point.shape, fill = point.fill) +
          ggplot2::scale_x_continuous("Theoretical Quantiles of the Normal Distribution", limits = xlim, breaks = xbreaks) +
          ggplot2::stat_qq_line(color = abline.col) +
          ggplot2::scale_y_continuous(switch(x$args$resid, unstand = "Sample Quantile of Unstandardized Residual", stand = "Sample Quantile of Standardized Residual"), limits = ylim, breaks = ybreaks) +
          ggplot2::facet_wrap(~ term, nrow = misty::uniq.n(plotdat2$term), scales = "free_y") +
          ggplot2::theme_bw() +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = -0.05, vjust = 2.5),
                         strip.text = ggplot2::element_text(size = strip.text.size),
                         axis.title = ggplot2::element_text(size = axis.title.size),
                         axis.text = ggplot2::element_text(size = axis.text.size)) +
          ggplot2::ggtitle("Level-2 Residuals")

        #···················
        #### Level-1 Histogram ####

        # Limits of the x-axis
        if (isTRUE(is.null(xlim))) { xlim.d <- c(-ceiling(max(abs(plotdat1$resid))), ceiling(max(abs(plotdat1$resid)))) }

        # Plot data
        p12 <- ggplot2::ggplot(plotdat1, ggplot2::aes(resid)) +
          ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), binwidth = hist.binwidth, bins = hist.bins, color = hist.col, fill = hist.fill) +
          ggplot2::stat_function(fun = dnorm, args = list(mean = mean(plotdat1$resid, na.rm = TRUE), sd = sd(plotdat1$resid, na.rm = TRUE)), col = function.col[1L], linetype = function.type[1L], linewidth = function.width[2L]) +
          ggplot2::geom_density(color = function.col[2L], linetype = function.type[2L], linewidth = function.width[2L]) +
          ggplot2::scale_x_continuous(switch(x$args$resid, unstand = "Unstandardized Residual", stand = "Standandardized Residual"), limits = if (isTRUE(is.null(xlim))) { xlim.d } else { xlim }, breaks = xbreaks) +
          ggplot2::scale_y_continuous("Density", limits = ylim, breaks = ybreaks) +
          ggplot2::theme_bw() +
          ggplot2::theme(strip.text = ggplot2::element_text(size = strip.text.size),
                         axis.title = ggplot2::element_text(size = axis.title.size),
                         axis.text = ggplot2::element_text(size = axis.text.size),
                         plot.margin = ggplot2::margin(5.5, 5.5, 20, 5.5, "pt"))

        #···················
        #### Level-2 Histogram ####

        # Limits of the x-axis
        if (isTRUE(is.null(xlim))) { xlim.d <- c(-ceiling(max(abs(plotdat2$condval))) - 0.1, ceiling(max(abs(plotdat2$condval))) + 0.1) }

        # Plot data
        p22 <- ggplot2::ggplot(plotdat2, ggplot2::aes(condval)) +
          ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), binwidth = hist.binwidth, bins = hist.bins, color = hist.col, fill = hist.fill) +
          ggplot2::stat_function(fun = dnorm, args = list(mean = mean(plotdat2$condval, na.rm = TRUE), sd = sd(plotdat2$condval, na.rm = TRUE)), col = function.col[1L], linetype = function.type[1L], linewidth = function.width[1L]) +
          ggplot2::geom_density(color = function.col[2L], linetype = function.type[2L], linewidth = function.width[2L]) +
          ggplot2::scale_x_continuous(switch(x$args$resid, unstand = "Unstandardized Residual", stand = "Standandardized Residual"),
                                      limits = if (isTRUE(is.null(xlim))) { xlim.d } else { xlim }, breaks = xbreaks) +
          ggplot2::scale_y_continuous("Density", limits = ylim, breaks = ybreaks) +
          ggplot2::facet_wrap(~ term, nrow = misty::uniq.n(plotdat2$term), scales = "free_y") +
          ggplot2::theme_bw() +
          ggplot2::theme(strip.text = ggplot2::element_text(size = strip.text.size),
                         axis.title = ggplot2::element_text(size = axis.title.size),
                         axis.text = ggplot2::element_text(size = axis.text.size))

        #···················
        #### Arrange Plots ####

        if (isTRUE(all(c("qq", "hist") %in% geom))) {

          p <- patchwork::wrap_plots(patchwork::wrap_plots(p11, p12, ncol = 2), patchwork::wrap_plots(p21, p22, ncol = 2), nrow = 2) + patchwork::plot_layout(heights = c(1L, misty::uniq.n(plotdat2$term)))

        } else if (isTRUE(all("qq" %in% geom))) {

          p <- patchwork::wrap_plots(p11, p21, nrow = 2) + patchwork::plot_layout(heights = c(1L, misty::uniq.n(plotdat2$term)))

        } else if (isTRUE(all("hist" %in% geom))) {

          p <- patchwork::wrap_plots(p12, p22, nrow = 2) + patchwork::plot_layout(heights = c(1L, misty::uniq.n(plotdat2$term)))

        }


      }

    })

  #_____________________________________________________________________________
  #
  # (Bootstrap) Confidence Intervals for Correlation Coefficients ——————————————

  }, ci.cor = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    .plot.argument(plot = "ci", geom = c("hist", "density", "point", "ci", "line"), point.size = 2.5, point.shape = 19L, errorbar.width = 0.3, errorbar.dodge.width = 0.5, hist.alpha = 0.4, hist.fill = "gray85", density.col = "#0072B2", density.width = 0.5, density.type = "solid", density.alpha = 0.4, point.col = "#CC79A7", point.width = 0.6, point.type = "solid", ci.col = "black", ci.width = 0.6, ci.type = "dashed", hline.intercept = 0L, hline.type = "solid", hline.col = "gray65", vline.intercept = 0L, vline.type = "solid", vline.col = "gray65", xbreaks = "ggplot2::waiver()", ybreaks = "ggplot2::waiver()", legend.title = "", legend.position = "bottom", legend.box.margin = c(-10L, 0L, 0L, 0L), facet.scales = "free_y", x = x, envir = environment())

    switch(plot, "ci" = { .plot.argument(ylab = "Correlation Coefficient", ylim = c(-1L, 1L), x = x, envir = environment()) }, "boot" = { .plot.argument(xlab = "Correlation Coefficient", ylab = "Probability Density, f(x)", xlim = c(-1L, 1L), x = x, envir = environment())     })

    if (isTRUE(is.null(plot.margin))) { if (isTRUE(is.null(x$data$group))) { plot.margin <- c(5.5, 5.5, 5.5, 5.5) } else { plot.margin <- c(5.5, 5.5, -2.5, 5.5) } }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Correlation Coefficient with Error Bars ####

    switch(plot, "ci" = {

      if (isTRUE(any(!is.na((if (isTRUE(!is.data.frame(x$result))) { do.call("rbind", x$result) } else { x$result })$low)))) {

        p <- .plot.ci(result = x$result, stat = "cor", group = x$data$group, split = x$data$split, point.size = point.size, point.shape = point.shape, errorbar.width = errorbar.width, errorbar.dodge.width = errorbar.dodge.width, line = "line" %in% geom, hline.intercept = hline.intercept, hline.type = hline.type, hline.col = hline.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        stop("There are no confidence intervals for the correlation coefficient to plot.", call. = FALSE)

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bootstrap Samples ####

    }, "boot" = {

      if (isTRUE(any(!is.na(x$boot$cor)))) {

        p <- .plot.boot(result = x$result, boot.sample = x$boot, stat = "cor", group = x$data$group, split = x$data$split, hist = "hist" %in% geom, hist.binwidth = hist.binwidth, hist.bins = hist.bins, hist.alpha = hist.alpha, hist.fill = hist.fill, density = "density" %in% geom, density.col = density.col, density.width = density.width, density.type = density.type, density.alpha = density.alpha, plot.point = "point" %in% geom, point.col = point.col, point.width = point.width, point.type = point.type, plot.ci = "ci" %in% geom, ci.col = ci.col, ci.width = ci.width, ci.type = ci.type, line = "line" %in% geom, vline.intercept = vline.intercept, vline.type = vline.type, vline.col = vline.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        stop("There are no bootstrap samples to plot.", call. = FALSE)

      }

    })

  #_____________________________________________________________________________
  #
  # (Bootstrap) Confidence Intervals for Arithmetic Means ——————————————————————

  }, ci.mean = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    .plot.argument(plot = "ci", geom = c("hist", "density", "point", "ci", "line"), point.size = 2.5, point.shape = 19L, errorbar.width = 0.3, errorbar.dodge.width = 0.5, hist.alpha = 0.4, hist.fill = "gray85", density.col = "#0072B2", density.width = 0.5, density.type = "solid", density.alpha = 0.4, point.col = "#CC79A7", point.width = 0.6, point.type = "solid", ci.col = "black", ci.width = 0.6, ci.type = "dashed", hline.intercept = 0L, hline.type = "solid", hline.col = "gray65", vline.intercept = 0L, vline.type = "solid", vline.col = "gray65", xbreaks = "ggplot2::waiver()", ybreaks = "ggplot2::waiver()", legend.title = "", legend.position = "bottom", legend.box.margin = c(-10L, 0L, 0L, 0L), facet.scales = "free_y", x = x, envir = environment())

    switch(plot, "ci" = { .plot.argument(ylab = "Arithmetic Mean", x = x, envir = environment()) }, "boot" = { .plot.argument(xlab = "Arithmetic Mean", ylab = "Probability Density, f(x)", x = x, envir = environment())     })

    if (isTRUE(is.null(plot.margin))) { if (isTRUE(is.null(x$data$group))) { plot.margin <- c(5.5, 5.5, 5.5, 5.5) } else { plot.margin <- c(5.5, 5.5, -2.5, 5.5) } }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Plot Arithmetic Mean with Error Bars ####

    switch(plot, "ci" = {

      if (isTRUE(any(!is.na((if (isTRUE(!is.data.frame(x$result))) { do.call("rbind", x$result) } else { x$result })$low)))) {

        p <- .plot.ci(result = x$result, stat = "m", group = x$data$group, split = x$data$split, point.size = point.size, point.shape = point.shape, errorbar.width = errorbar.width, errorbar.dodge.width = errorbar.dodge.width, line = "line" %in% geom, hline.intercept = hline.intercept, hline.type = hline.type, hline.col = hline.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        plot <- "none"

        warning("There are no confidence intervals for the arithmetic mean to plot.", call. = FALSE)

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Plot Bootstrap Samples ####

    }, "boot" = {

      if (any(!is.na(x$boot$m))) {

        p <- .plot.boot(result = x$result, boot.sample = x$boot, stat = "m", group = x$data$group, split = x$data$split, hist = "hist" %in% geom, hist.binwidth = hist.binwidth, hist.bins = hist.bins, hist.alpha = hist.alpha, hist.fill = hist.fill, density = "density" %in% geom, density.col = density.col, density.width = density.width, density.type = density.type, density.alpha = density.alpha, plot.point = "point" %in% geom, point.col = point.col, point.width = point.width, point.type = point.type, plot.ci = "ci" %in% geom, ci.col = ci.col, ci.width = ci.width, ci.type = ci.type, line = "line" %in% geom, vline.intercept = vline.intercept, vline.type = vline.type, vline.col = vline.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        plot <- "none"

        warning("There are no bootstrap samples to plot.", call. = FALSE)

      }

    })

  #_____________________________________________________________________________
  #
  # (Bootstrap) Confidence Intervals for Median ————————————————————————————————

  }, ci.median = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    .plot.argument(plot = "ci", geom = c("hist", "density", "point", "ci", "line"), point.size = 2.5, point.shape = 19L, errorbar.width = 0.3, errorbar.dodge.width = 0.5, hist.alpha = 0.4, hist.fill = "gray85", density.col = "#0072B2", density.width = 0.5, density.type = "solid", density.alpha = 0.4, point.col = "#CC79A7", point.width = 0.6, point.type = "solid", ci.col = "black", ci.width = 0.6, ci.type = "dashed", hline.intercept = 0L, hline.type = "solid", hline.col = "gray65", vline.intercept = 0L, vline.type = "solid", vline.col = "gray65", xbreaks = "ggplot2::waiver()", ybreaks = "ggplot2::waiver()", legend.title = "", legend.position = "bottom", legend.box.margin = c(-10L, 0L, 0L, 0L), facet.scales = "free_y", x = x, envir = environment())

    switch(plot, "ci" = { .plot.argument(ylab = "Median", x = x, envir = environment()) }, "boot" = { .plot.argument(xlab = "Median", ylab = "Probability Density, f(x)", x = x, envir = environment())     })

    if (isTRUE(is.null(plot.margin))) { if (isTRUE(is.null(x$data$group))) { plot.margin <- c(5.5, 5.5, 5.5, 5.5) } else { plot.margin <- c(5.5, 5.5, -2.5, 5.5) } }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Plot Median with Error Bars ####

    switch(plot, "ci" = {

      if (isTRUE(any(!is.na((if (isTRUE(!is.data.frame(x$result))) { do.call("rbind", x$result) } else { x$result })$low)))) {

        p <- .plot.ci(result = x$result, stat = "med", group = x$data$group, split = x$data$split, point.size = point.size, point.shape = point.shape, errorbar.width = errorbar.width, errorbar.dodge.width = errorbar.dodge.width, line = "line" %in% geom, hline.intercept = hline.intercept, hline.type = hline.type, hline.col = hline.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        warning("There are no confidence intervals for the median to plot.", call. = FALSE)

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Plot Bootstrap Samples ####

    }, "boot" = {

      if (any(!is.na(x$boot$med))) {

        p <- .plot.boot(result = x$result, boot.sample = x$boot, stat = "med", group = x$data$group, split = x$data$split, hist = "hist" %in% geom, hist.binwidth = hist.binwidth, hist.bins = hist.bins, hist.alpha = hist.alpha, hist.fill = hist.fill, density = "density" %in% geom, density.col = density.col, density.width = density.width, density.type = density.type, density.alpha = density.alpha, plot.point = "point" %in% geom, point.col = point.col, point.width = point.width, point.type = point.type, plot.ci = "ci" %in% geom, ci.col = ci.col, ci.width = ci.width, ci.type = ci.type, line = "line" %in% geom, vline.intercept = vline.intercept, vline.type = vline.type, vline.col = vline.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        warning("There are no bootstrap samples to plot.", call. = FALSE)

      }

    })

  #_____________________________________________________________________________
  #
  # (Bootstrap) Confidence Intervals for Proportions ———————————————————————————

  }, ci.prop = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    .plot.argument(plot = "ci", geom = c("hist", "density", "point", "ci", "line"), point.size = 2.5, point.shape = 19L, errorbar.width = 0.3, errorbar.dodge.width = 0.5, hist.alpha = 0.4, hist.fill = "gray85", density.col = "#0072B2", density.width = 0.5, density.type = "solid", density.alpha = 0.4, point.col = "#CC79A7", point.width = 0.6, point.type = "solid", ci.col = "black", ci.width = 0.6, ci.type = "dashed", hline.intercept = 0L, hline.type = "solid", hline.col = "gray65", vline.intercept = 0L, vline.type = "solid", vline.col = "gray65", xbreaks = "ggplot2::waiver()", ybreaks = "ggplot2::waiver()", legend.title = "", legend.position = "bottom", legend.box.margin = c(-10L, 0L, 0L, 0L), facet.scales = "free_y", x = x, envir = environment())

    switch(plot, "ci" = { .plot.argument(ylab = "Proportion", ylim = c(0L, 1L), x = x, envir = environment()) }, "boot" = { .plot.argument(xlab = "Proportion", ylab = "Probability Density, f(x)", xlim = c(0L, 1L), x = x, envir = environment())     })

    if (isTRUE(is.null(plot.margin))) { if (isTRUE(is.null(x$data$group))) { plot.margin <- c(5.5, 5.5, 5.5, 5.5) } else { plot.margin <- c(5.5, 5.5, -2.5, 5.5) } }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Proportion with Error Bars ####

    switch(plot, "ci" = {

      if (isTRUE(any(!is.na((if (isTRUE(!is.data.frame(x$result))) { do.call("rbind", x$result) } else { x$result })$low)))) {

        p <- .plot.ci(result = x$result, stat = "prop", group = x$data$group, split = x$data$split, point.size = point.size, point.shape = point.shape, errorbar.width = errorbar.width, errorbar.dodge.width = errorbar.dodge.width, line = "line" %in% geom, hline.intercept = hline.intercept, hline.type = hline.type, hline.col = hline.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        warning("There are no confidence intervals for the proportion to plot.", call. = FALSE)

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bootstrap Samples ####

    }, "boot" = {

      if (any(!is.na(x$boot$prop))) {

        p <- .plot.boot(result = x$result, boot.sample = x$boot, stat = "prop", group = x$data$group, split = x$data$split, hist = "hist" %in% geom, hist.binwidth = hist.binwidth, hist.bins = hist.bins, hist.alpha = hist.alpha, hist.fill = hist.fill, density = "density" %in% geom, density.col = density.col, density.width = density.width, density.type = density.type, density.alpha = density.alpha, plot.point = "point" %in% geom, point.col = point.col, point.width = point.width, point.type = point.type, plot.ci = "ci" %in% geom, ci.col = ci.col, ci.width = ci.width, ci.type = ci.type, line = "line" %in% geom, vline.intercept = vline.intercept, vline.type = vline.type, vline.col = vline.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        warning("There are no bootstrap samples to plot.", call. = FALSE)

      }

    })

  #_____________________________________________________________________________
  #
  # (Bootstrap) Confidence Intervals for Variances —————————————————————————————

  }, ci.var = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    .plot.argument(plot = "ci", geom = c("hist", "density", "point", "ci", "line"), point.size = 2.5, point.shape = 19L, errorbar.width = 0.3, errorbar.dodge.width = 0.5, hist.alpha = 0.4, hist.fill = "gray85", density.col = "#0072B2", density.width = 0.5, density.type = "solid", density.alpha = 0.4, point.col = "#CC79A7", point.width = 0.6, point.type = "solid", ci.col = "black", ci.width = 0.6, ci.type = "dashed", hline.intercept = 0L, hline.type = "solid", hline.col = "gray65", vline.intercept = 0L, vline.type = "solid", vline.col = "gray65", xbreaks = "ggplot2::waiver()", ybreaks = "ggplot2::waiver()", legend.title = "", legend.position = "bottom", legend.box.margin = c(-10L, 0L, 0L, 0L), facet.scales = "free_y", x = x, envir = environment())

    switch(plot, "ci" = { .plot.argument(ylab = "Variance", x = x, envir = environment()) }, "boot" = { .plot.argument(xlab = "Variance", ylab = "Probability Density, f(x)", x = x, envir = environment())     })

    if (isTRUE(is.null(plot.margin))) { if (isTRUE(is.null(x$data$group))) { plot.margin <- c(5.5, 5.5, 5.5, 5.5) } else { plot.margin <- c(5.5, 5.5, -2.5, 5.5) } }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Variance with Error Bars ####

    switch(plot, "ci" = {

      if (isTRUE(any(!is.na((if (isTRUE(!is.data.frame(x$result))) { do.call("rbind", x$result) } else { x$result })$low)))) {

        p <- .plot.ci(result = x$result, stat = "var", group = x$data$group, split = x$data$split, point.size = point.size, point.shape = point.shape, errorbar.width = errorbar.width, errorbar.dodge.width = errorbar.dodge.width, line = "line" %in% geom, hline.intercept = hline.intercept, hline.type = hline.type, hline.col = hline.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        warning("There are no confidence intervals for the variance to plot.", call. = FALSE)

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bootstrap Samples ####

    }, "boot" = {

      if (any(!is.na(x$boot$var))) {

        p <- .plot.boot(result = x$result, boot.sample = x$boot, stat = "var", group = x$data$group, split = x$data$split, hist = "hist" %in% geom, hist.binwidth = hist.binwidth, hist.bins = hist.bins, hist.alpha = hist.alpha, hist.fill = hist.fill, density = "density" %in% geom, density.col = density.col, density.width = density.width, density.type = density.type, density.alpha = density.alpha, plot.point = "point" %in% geom, point.col = point.col, point.width = point.width, point.type = point.type, plot.ci = "ci" %in% geom, ci.col = ci.col, ci.width = ci.width, ci.type = ci.type, line = "line" %in% geom, vline.intercept = vline.intercept, vline.type = vline.type, vline.col = vline.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        warning("There are no bootstrap samples to plot.", call. = FALSE)

      }

    })

  #_____________________________________________________________________________
  #
  # (Bootstrap) Confidence Intervals for Standard Deviations ———————————————————

  }, ci.sd = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    .plot.argument(plot = "ci", geom = c("hist", "density", "point", "ci", "line"), point.size = 2.5, point.shape = 19L, errorbar.width = 0.3, errorbar.dodge.width = 0.5, hist.alpha = 0.4, hist.fill = "gray85", density.col = "#0072B2", density.width = 0.5, density.type = "solid", density.alpha = 0.4, point.col = "#CC79A7", point.width = 0.6, point.type = "solid", ci.col = "black", ci.width = 0.6, ci.type = "dashed", hline.intercept = 0L, hline.type = "solid", hline.col = "gray65", vline.intercept = 0L, vline.type = "solid", vline.col = "gray65", xbreaks = "ggplot2::waiver()", ybreaks = "ggplot2::waiver()", legend.title = "", legend.position = "bottom", legend.box.margin = c(-10L, 0L, 0L, 0L), facet.scales = "free_y", x = x, envir = environment())

    switch(plot, "ci" = { .plot.argument(ylab = "Standard Deviation", x = x, envir = environment()) }, "boot" = { .plot.argument(xlab = "Standard Deviation", ylab = "Probability Density, f(x)", x = x, envir = environment())     })

    if (isTRUE(is.null(plot.margin))) { if (isTRUE(is.null(x$data$group))) { plot.margin <- c(5.5, 5.5, 5.5, 5.5) } else { plot.margin <- c(5.5, 5.5, -2.5, 5.5) } }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Standard Deviation with Error Bars ####

    switch(plot, "ci" = {

      if (isTRUE(any(!is.na((if (isTRUE(!is.data.frame(x$result))) { do.call("rbind", x$result) } else { x$result })$low)))) {

        p <- .plot.ci(result = x$result, stat = "sd", group = x$data$group, split = x$data$split, point.size = point.size, point.shape = point.shape, errorbar.width = errorbar.width, errorbar.dodge.width = errorbar.dodge.width, line = "line" %in% geom, hline.intercept = hline.intercept, hline.type = hline.type, hline.col = hline.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        warning("There are no confidence intervals for the standard deviation to plot.", call. = FALSE)

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bootstrap Samples ####

    }, "boot" = {

      if (any(!is.na(x$boot$sd))) {

        p <- .plot.boot(result = x$result, boot.sample = x$boot, stat = "sd", group = x$data$group, split = x$data$split, hist = "hist" %in% geom, hist.binwidth = hist.binwidth, hist.bins = hist.bins, hist.alpha = hist.alpha, hist.fill = hist.fill, density = "density" %in% geom, density.col = density.col, density.width = density.width, density.type = density.type, density.alpha = density.alpha, plot.point = "point" %in% geom, point.col = point.col, point.width = point.width, point.type = point.type, plot.ci = "ci" %in% geom, ci.col = ci.col, ci.width = ci.width, ci.type = ci.type, line = "line" %in% geom, vline.intercept = vline.intercept, vline.type = vline.type, vline.col = vline.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        warning("There are no bootstrap samples to plot.", call. = FALSE)

      }

    })


  #_____________________________________________________________________________
  #
  # Correlation Matrix —————————————————————————————————————————————————————————

  }, cor.matrix = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    .plot.argument(tile.col = "black", text.size = c(3.5, 3.5), scale.fill.low = "#D41159", scale.fill.high = "#1A85FF", scale.fill.mid = "white", x = x, envir = environment())

    if (isTRUE(is.null(text.col))) {

      if (isTRUE(x$args$color != "default")) {

        text.col <- c("black", if (isTRUE(!x$args$color %in% c("black", "red", "green", "yellow", "blue", "violet", "cyan"))) {

          switch(x$args$color, "gray1" = "#5b5d59", "gray2" = "#d3d7cf", "gray3" = "#eeeeec", "b.red" = "#ef3232", "b.green" = "#8ae234", "b.yellow" = "#fce94f", "b.blue" = "#729fcf", "b.violet" = "#ad7fa8", "b.cyan" = "#34e2e2", "b.white" = "#eeeeec")

        } else {

          x$args$color

        })

      } else {

        text.col <- "black"

      }

    }

    if (isTRUE(is.null(text.font))) { text.font <- c("plain", ifelse(x$args$style == "regular", "plain", x$args$style)) }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Plot Data ####

    # Correlation and significance value matrix
    cordat <- x$result$cor
    sigdat <- x$result$p

    # Grouping
    if (isTRUE(".group" %in% colnames(x$data))) {

      cordat[lower.tri(cordat)]  <- t(x$result$cor)[lower.tri(cordat)]
      cordat[upper.tri(cordat)] <- t(x$result$cor)[upper.tri(cordat)]

      sigdat[lower.tri(sigdat)] <- t(x$result$p)[lower.tri(sigdat)]
      sigdat[upper.tri(sigdat)] <- t(x$result$p)[upper.tri(sigdat)]

    }

    # Triangular of the matrix
    switch(x$args$tri, "lower" = {

      cordat[lower.tri(cordat)] <- NA
      sigdat[lower.tri(sigdat)] <- NA

    }, "upper" = {

      cordat[upper.tri(cordat)] <- NA
      sigdat[upper.tri(sigdat)] <- NA

    })

    # Plot data
    plotdat <- as.data.frame(as.table(cordat)) |> (\(p) setNames(data.frame(p, cor.format = formatC(p$Freq, digits = x$args$digits, format = "f"), sig = as.data.frame(as.table(sigdat))$Freq < x$args$alpha), c("x", "y", "cor","cor.format", "sig")))() |> (\(q) q[!is.na(q$cor), ])()

    # Factors
    plotdat$x <- factor(plotdat$x, levels = colnames(x$result$cor))
    plotdat$y <- factor(plotdat$y, levels = colnames(x$result$cor))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Create ggplot ####

    p <- ggplot2::ggplot(plotdat, ggplot2::aes(x, y, fill = cor)) +
      ggplot2::geom_tile(color = tile.col) +
      ggplot2::theme_bw() + ggplot2::coord_fixed() +
      ggplot2::scale_fill_gradient2(low = scale.fill.low, high = scale.fill.high, mid = scale.fill.mid, na.value = "white", midpoint = 0L, limit = c(-1L, 1L)) +
      ggplot2::scale_x_discrete(expand = c(0L, 0L)) + ggplot2::scale_y_discrete(expand = c(0L, 0L), limits = rev) +
      ggplot2::guides(fill = ggplot2::guide_colorbar(draw.ulim = FALSE, draw.llim = FALSE, theme = ggplot2::theme(legend.key.width = ggplot2::unit(1L, "lines"), legend.key.height = ggplot2::unit(20L, "lines")))) +
      ggplot2::theme(legend.title = ggplot2::element_blank(), axis.title = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     axis.text = ggplot2::element_text(size = axis.text.size))

    # Add correlation coefficients inside every tile
    if (isTRUE(length(text.col) == 2L)) {

      p <- p +
        # Non-significant correlations
        ggplot2::geom_text(data = subset(plotdat, !sig), ggplot2::aes(label = cor.format), color = text.col[1L], size = text.size[1L], fontface = text.font[1L]) +
        # Significant correlations
        ggplot2::geom_text(data = subset(plotdat,  sig), ggplot2::aes(label = cor.format), color = text.col[2L], size = text.size[2L], fontface = text.font[2L])

      } else {

        p <- p + ggplot2::geom_text(ggplot2::aes(label = cor.format), color = text.col[1L], size = text.size[1L], fontface = text.font[1L])

      }

  #_____________________________________________________________________________
  #
  # Dynamic Fit Index Cutoffs ——————————————————————————————————————————————————

  }, item.dfi = {

    fit <- cutoff <- NULL

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Data ####

    #—————————————————————————————————————— #
    ### Level 0 ####

    if (isTRUE(length(x$result$fit.sim) == 1L)) {

      df <- data.frame(level = "Level 0", group = "True",
                       index = factor(rep(c("CFI", "TLI", "RMSEA", "SRMR"), each = x$args$nrep), levels = c("CFI", "TLI", "RMSEA", "SRMR")),
                       fit = unlist(x$result$fit.sim[["Level 0"]]),
                       cutoff = rep( unlist(x$result$fit.cutoff["Level 0", c("cfi", "tli", "rmsea", "srmr")]), each = x$args$nrep))


    #—————————————————————————————————————— #
    ### Level 0, 1, 2, and/or 3 ####

    } else {

      df <- NULL
      for (i in setdiff(names(x$result$fit.sim), "Level 0")) {

        df <- rbind(df,
                    data.frame(level = i,
                               rbind(data.frame(group = "True",
                                                index = factor(rep(c("CFI", "TLI", "RMSEA", "SRMR"), each = x$args$nrep), levels = c("CFI", "TLI", "RMSEA", "SRMR")),
                                                fit = unlist(x$result$fit.sim[["Level 0"]]),
                                                cutoff = rep( unlist(x$result$fit.cutoff["Level 0", c("cfi", "tli", "rmsea", "srmr")]), each = x$args$nrep)),
                                     data.frame(group = "Misspecified",
                                                index = factor(rep(c("CFI", "TLI", "RMSEA", "SRMR"), each = x$args$nrep), levels = c("CFI", "TLI", "RMSEA", "SRMR")),
                                                fit = unlist(x$result$fit.sim[[i]]),
                                                cutoff = rep( unlist(x$result$fit.cutoff[i, c("cfi", "tli", "rmsea", "srmr")]), each = x$args$nrep)))))

      }

      df$group <- factor(df$group, levels = c("True", "Misspecified"))

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Create ggplot ####

    p <- ggplot2::ggplot(df, ggplot2::aes(fit, group = group, color = group, fill = group)) +
          ggplot2::theme_bw() +
          ggplot2::geom_histogram(position = "identity", color = "gray60", alpha = 0.5) +
          ggplot2::geom_vline(ggplot2::aes(xintercept = cutoff, group = group, color = group), linetype = "longdash") +
          ggplot2::scale_fill_manual(values = c("#56B4E9", "#E9798C"))+
          ggplot2::scale_color_manual(values = c("#56B4E9", "#E9798C"))+
          ggplot2::facet_grid(level ~ index,  scales = "free_x" ) +
          ggplot2::scale_x_continuous(name = "")  +
          ggplot2::scale_y_continuous(name = "Count", expand = ggplot2::expansion(mult = c(0L, 0.05)))  +
          ggplot2::theme(legend.position = "bottom",
                         legend.box.margin = ggplot2::margin(-20L, 0L, 0L, 0L),
                         legend.title = ggplot2::element_blank(),
                         legend.background = ggplot2::element_rect(fill = "transparent"),
                         strip.text = ggplot2::element_text(size = 11))

  #_____________________________________________________________________________
  #
  # Within-Group and Between-Group Correlation Matrix ——————————————————————————

  }, multilevel.cor = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    .plot.argument(tile.col = "black", text.size = c(3.5, 3.5), scale.fill.low = "#D41159", scale.fill.high = "#1A85FF", scale.fill.mid = "white", x = x, envir = environment())

    if (isTRUE(is.null(text.col))) {

      if (isTRUE(x$args$color != "default")) {

        text.col <- c("black", if (isTRUE(!x$args$color %in% c("black", "red", "green", "yellow", "blue", "violet", "cyan"))) {

          switch(x$args$color, "gray1" = "#5b5d59", "gray2" = "#d3d7cf", "gray3" = "#eeeeec", "b.red" = "#ef3232", "b.green" = "#8ae234", "b.yellow" = "#fce94f", "b.blue" = "#729fcf", "b.violet" = "#ad7fa8", "b.cyan" = "#34e2e2", "b.white" = "#eeeeec")

        } else {

          x$args$color

        })

      } else {

        text.col <- "black"

      }

    }

    if (isTRUE(is.null(text.font))) { text.font <- c("plain", ifelse(x$args$style == "regular", "plain", x$args$style)) }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Plot Data ####

    #—————————————————————————————————————— #
    ### Within-Group and Between-Group Correlation Matrix ####

    if (isTRUE(!x$args$split)) {

      # Correlation and significance value matrix
      cordat <- x$result$wb.cor
      sigdat <- x$result$wb.p

      cordat[lower.tri(cordat)]  <- t(x$result$wb.cor)[lower.tri(cordat)]
      cordat[upper.tri(cordat)] <- t(x$result$wb.cor)[upper.tri(cordat)]

      sigdat[lower.tri(sigdat)] <- t(x$result$wb.p)[lower.tri(sigdat)]
      sigdat[upper.tri(sigdat)] <- t(x$result$wb.p)[upper.tri(sigdat)]

      diag(cordat) <- diag(sigdat) <- NA

      # Plot data
      plotdat <- as.data.frame(as.table(cordat)) |> (\(p) setNames(data.frame(p, cor.format = formatC(p$Freq, digits = x$args$digits, format = "f"), sig = as.data.frame(as.table(sigdat))$Freq < x$args$alpha), c("x", "y", "cor","cor.format", "sig")))() |> (\(q) q[!is.na(q$cor), ])()

      # Factors
      plotdat$x <- factor(plotdat$x, levels = colnames(x$result$wb.cor))
      plotdat$y <- factor(plotdat$y, levels = colnames(x$result$wb.cor))

      #···················
      #### Create ggplot ####

      p <- ggplot2::ggplot(plotdat, ggplot2::aes(x, y, fill = cor)) +
        ggplot2::geom_tile(color = tile.col) +
        ggplot2::theme_bw() + ggplot2::coord_fixed() +
        ggplot2::scale_fill_gradient2(low = scale.fill.low, high = scale.fill.high, mid = scale.fill.mid, na.value = "white", midpoint = 0L, limit = c(-1L, 1L)) +
        ggplot2::scale_x_discrete(expand = c(0L, 0L)) + ggplot2::scale_y_discrete(expand = c(0L, 0L), limits = rev) +
        ggplot2::labs(caption = if (isTRUE(x$args$tri.lower)) { "Note. Lower triangular: Within-Group, Upper triangular: Between-Group" } else { "Note. Lower triangular: Between-Group, Upper triangular: Within-Group" }) +
        ggplot2::guides(fill = ggplot2::guide_colorbar(draw.ulim = FALSE, draw.llim = FALSE, theme = ggplot2::theme(legend.key.width = ggplot2::unit(1L, "lines"), legend.key.height = ggplot2::unit(20L, "lines")))) +
        ggplot2::theme(legend.title = ggplot2::element_blank(), axis.title = ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_blank(),
                       axis.text = ggplot2::element_text(size = axis.text.size),
                       plot.caption = ggplot2::element_text(hjust = 0.5))

    #—————————————————————————————————————— #
    ### Split Within-Group and Between-Group Correlation Matrix ####

    } else {

      # Correlation and significance value matrix
      cordat.w <- x$result$with.cor
      cordat.b <- x$result$betw.cor

      sigdat.w <- x$result$with.p
      sigdat.b <- x$result$betw.p

      cordat.w[lower.tri(cordat.w)] <- t(x$result$with.cor)[lower.tri(cordat.w)]
      cordat.w[upper.tri(cordat.w)] <- t(x$result$with.cor)[upper.tri(cordat.w)]

      cordat.b[lower.tri(cordat.b)] <- t(x$result$betw.cor)[lower.tri(cordat.b)]
      cordat.b[upper.tri(cordat.b)] <- t(x$result$betw.cor)[upper.tri(cordat.b)]

      sigdat.w[lower.tri(sigdat.w)] <- t(x$result$with.p)[lower.tri(sigdat.w)]
      sigdat.w[upper.tri(sigdat.w)] <- t(x$result$with.p)[upper.tri(sigdat.w)]

      sigdat.b[lower.tri(sigdat.b)] <- t(x$result$betw.p)[lower.tri(sigdat.b)]
      sigdat.b[upper.tri(sigdat.b)] <- t(x$result$betw.p)[upper.tri(sigdat.b)]

      diag(cordat.w) <- diag(cordat.b) <- diag(sigdat.w) <- diag(sigdat.b) <- NA

      # Triangular of the matrix
      switch(x$args$tri, "lower" = {

        cordat.w[lower.tri(cordat.w)] <- cordat.b[lower.tri(cordat.b)] <- NA
        sigdat.w[lower.tri(sigdat.w)] <- sigdat.b[lower.tri(sigdat.b)] <- NA

      }, "upper" = {

        cordat.w[upper.tri(cordat.w)] <- cordat.b[upper.tri(cordat.b)] <- NA
        sigdat.w[upper.tri(sigdat.w)] <- sigdat.b[upper.tri(sigdat.b)] <- NA


      })
      # Plot data
      plotdat <- rbind(data.frame(group = "Within", as.data.frame(as.table(cordat.w)) |> (\(p) setNames(data.frame(p, cor.format = formatC(p$Freq, digits = x$args$digits, format = "f"), sig = as.data.frame(as.table(sigdat.w))$Freq < x$args$alpha), c("x", "y", "cor","cor.format", "sig")))() |> (\(q) q[!is.na(q$cor), ])()),
                       data.frame(group = "Between", as.data.frame(as.table(cordat.b)) |> (\(p) setNames(data.frame(p, cor.format = formatC(p$Freq, digits = x$args$digits, format = "f"), sig = as.data.frame(as.table(sigdat.b))$Freq < x$args$alpha), c("x", "y", "cor","cor.format", "sig")))() |> (\(q) q[!is.na(q$cor), ])()))

      # Factors
      plotdat$group <- factor(plotdat$group, levels = c("Within", "Between"))

      plotdat$x <- factor(plotdat$x, levels = unique(c(colnames(x$result$with.cor), colnames(x$result$betw.cor))))
      plotdat$y <- factor(plotdat$y, levels = unique(c(colnames(x$result$with.cor), colnames(x$result$betw.cor))))

      #···················
      #### Create ggplot ####

      p <- ggplot2::ggplot(plotdat, ggplot2::aes(x, y, fill = cor)) +
        ggplot2::geom_tile(color = tile.col) +
        ggplot2::facet_wrap(~ group, nrow = 2L) +
        ggplot2::theme_bw() + ggplot2::coord_fixed() +
        ggplot2::scale_fill_gradient2(low = scale.fill.low, high = scale.fill.high, mid = scale.fill.mid, na.value = "white", midpoint = 0L, limit = c(-1L, 1L)) +
        ggplot2::scale_x_discrete(expand = c(0L, 0L)) + ggplot2::scale_y_discrete(expand = c(0L, 0L), limits = rev) +
        ggplot2::guides(fill = ggplot2::guide_colorbar(draw.ulim = FALSE, draw.llim = FALSE, theme = ggplot2::theme(legend.key.width = ggplot2::unit(1L, "lines"), legend.key.height = ggplot2::unit(20L, "lines")))) +
        ggplot2::theme(legend.title = ggplot2::element_blank(), axis.title = ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_blank(),
                       axis.text = ggplot2::element_text(size = axis.text.size),
                       strip.text = ggplot2::element_text(size = strip.text.size))

    }

    # Add correlation coefficients inside every tile
    if (isTRUE(length(text.col) == 2L)) {

      p <- p +
        # Non-significant correlations
        ggplot2::geom_text(data = subset(plotdat, !sig), ggplot2::aes(label = cor.format), color = text.col[1L], size = text.size[1L], fontface = text.font[1L]) +
        # Significant correlations
        ggplot2::geom_text(data = subset(plotdat,  sig), ggplot2::aes(label = cor.format), color = text.col[2L], size = text.size[2L], fontface = text.font[2L])

    } else {

      p <- p + ggplot2::geom_text(ggplot2::aes(label = cor.format), color = text.col[1L], size = text.size[1L], fontface = text.font[1L])

    }

  #_____________________________________________________________________________
  #
  # R-Squared Measures for Multilevel and LMMs —————————————————————————————————

  }, multilevel.r2 = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    .plot.argument(gray = FALSE, start = 0.15, end = 0.85, scale.fill.col = c("#D55E00", "#0072B2", "#CC79A7", "#009E73", "#E69F00"), x = x, envir = environment())

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Predictors Not Cluster-Mean-Centered ####

    # Predictors are not cluster-mean-centered
    if (isTRUE(ncol(x$result$rs$decomp) == 1L)) {

      df <- data.frame(var = factor(rep("Total", times = 4L)),
                       part = factor(c("Fixed Slopes", "Slope Variation", "Intercept Variation", "Residual"), levels = c("Residual", "Intercept Variation", "Slope Variation", "Fixed Slopes")),
                       y = as.vector(x$result$rs$decomp))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Predictors Cluster-Mean-Centered ####

    } else {

      df <- data.frame(var = factor(rep(c("Total", "Within", "Between"), each = 5L), levels = c("Total", "Within", "Between")),
                       part = factor(c("Fixed Slopes (Within)", "Fixed Slopes (Between)","Slope Variation (Within)", "Intercept Variation (Between)", "Residual (Within)"), levels = c("Residual (Within)", "Intercept Variation (Between)", "Slope Variation (Within)", "Fixed Slopes (Between)", "Fixed Slopes (Within)")),
                       y = as.vector(x$result$rs$decomp))

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Plot ####

    p <- ggplot2::ggplot(df, ggplot2::aes(x = var, y = y, fill = part)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_y_continuous(name = "Proportion of Variance", breaks = seq(0L, 1L, by = 0.1)) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.title = ggplot2::element_blank(),
                     legend.position = "bottom",
                     legend.box.margin = ggplot2::margin(-10L, 6L, 6L, 6L)) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2L, reverse = TRUE))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Gray Color Scales ####

    if (isTRUE(gray)) {

      p <- p + ggplot2::scale_fill_grey(start = end, end = start)

    } else {

      p <- p + ggplot2::scale_fill_manual(values = rev(scale.fill.col))

    }

  #_____________________________________________________________________________
  #
  # R-Squared Measures for Multilevel and LMM, Manual Input ————————————————————

  }, multilevel.r2.manual = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    .plot.argument(gray = FALSE, start = 0.15, end = 0.85, scale.fill.col = c("#D55E00", "#0072B2", "#CC79A7", "#009E73", "#E69F00"), x = x, envir = environment())

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Predictors Not Cluster-Mean-Centered ####

    # Predictors are not cluster-mean-centered
    if (isTRUE(ncol(x$result$decomp) == 1L)) {

      df <- data.frame(var = factor(rep("Total", times = 4L)),
                       part = factor(c("Fixed Slopes", "Slope Variation", "Intercept Variation", "Residual"), levels = c("Residual", "Intercept Variation", "Slope Variation", "Fixed Slopes")),
                       y = as.vector(x$result$decomp))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Predictors Cluster-Mean-Centered ####

    } else {

      df <- data.frame(var = factor(rep(c("Total", "Within", "Between"), each = 5L), levels = c("Total", "Within", "Between")),
                       part = factor(c("Fixed Slopes (Within)", "Fixed Slopes (Between)","Slope Variation (Within)", "Intercept Variation (Between)", "Residual (Within)"), levels = c("Residual (Within)", "Intercept Variation (Between)", "Slope Variation (Within)", "Fixed Slopes (Between)", "Fixed Slopes (Within)")),
                       y = as.vector(x$result$decomp))

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Create ggplot ####

    p <- ggplot2::ggplot(df, ggplot2::aes(x = var, y = y, fill = part)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_y_continuous(name = "Proportion of Variance", breaks = seq(0L, 1L, by = 0.1)) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.title = ggplot2::element_blank(),
                     legend.position = "bottom",
                     legend.box.margin = ggplot2::margin(-10L, 6L, 6L, 6L)) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2L, reverse = TRUE))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Gray Color Scales ####

    if (isTRUE(gray)) {

      p <- p + ggplot2::scale_fill_grey(start = end, end = start)

    } else {

      p <- p + ggplot2::scale_fill_manual(values = rev(scale.fill.col))

    }

  #_____________________________________________________________________________
  #
  # Missing Data Pattern ———————————————————————————————————————————————————————

  }, na.pattern = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    .plot.argument(geom = "square", tile.alpha = 0.6, scale.fill.col = c("#B61A51B3", "#006CC2B3"), plot.margin = c(4L, 16L, 0L, 4L), legend.box.margin = c(-8L, 6L, 6L, 6L), legend.key.size = 12L, legend.text.size = 9L, x = x, envir = environment())

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Plot Data ####

    plotdat <- do.call("rbind", apply(x$result[-nrow(x$result), c("pattern", colnames(x$data))], 1L, function(y) {

      data.frame(pattern = y["pattern"], var = colnames(x$data), obs_miss = y[colnames(x$data)], x = seq_len(ncol(x$data)), row.names = NULL)

    }))

    # Factor
    plotdat$obs_miss <- factor(ifelse(plotdat$obs_miss == 1L, "Observed", "Missing"))

    # Patterns excluded
    if (isTRUE(!is.null(x$args$n.pattern))) {

      n.pattern.exclude <- names(which(table(apply(is.na(x$data), 1L, function(y) paste(as.numeric(y), collapse = ""))) < x$args$n.pattern))

    } else {

      n.pattern.exclude <- NULL

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Create ggplot ####

    p <- ggplot2::ggplot(plotdat, ggplot2::aes(x, pattern, fill = obs_miss, alpha = tile.alpha)) +
      ggplot2::geom_tile(color = "black") +
      ggplot2::scale_fill_manual(values = c("Missing" = scale.fill.col[1L], "Observed" = scale.fill.col[2L])) +
      ggplot2::scale_alpha_continuous(limits = c(0, 1), guide = "none") +
      ggplot2::scale_x_continuous("Number of Missing Entries per Variable", breaks = seq_len(length(colnames(x$data))),
                                  labels = as.character(x$result[nrow(x$result), colnames(x$data)]),
                                  sec.axis = ggplot2::dup_axis(labels = colnames(x$data), name = "Variable")) +
      ggplot2::scale_y_reverse("Pattern Frequency", breaks = seq_len(length(x$result$n) - 1L), labels = x$result[-nrow(x$result), "n"],
                                sec.axis = ggplot2::dup_axis(labels = x$result[-nrow(x$result), "nNA"], name = "Number of Missing Entries per Pattern")) +
      ggplot2::theme(plot.margin = ggplot2::margin(t = plot.margin[1L], r = plot.margin[2L], b = plot.margin[3L], l = plot.margin[4L]),
                     legend.title = ggplot2::element_blank(), legend.position = "bottom", legend.key.size = ggplot2::unit(legend.key.size, "pt"),
                     legend.text = ggplot2::element_text(size = legend.text.size), legend.box.margin = ggplot2::margin(t = legend.box.margin[1L], r = legend.box.margin[2L], b = legend.box.margin[3L], l = legend.box.margin[4L]),
                     panel.grid.minor = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank())

    ### Square Plot Tiles ####
    if (isTRUE("square" %in% geom)) { p <- p + ggplot2::coord_fixed(expand = FALSE) } else { p <- p + ggplot2::coord_cartesian(expand = FALSE) }

    ### Rotate Labels ####
    if (isTRUE("rotate" %in% geom)) { p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90L)) }

    ### Caption ####
    if (isTRUE(length(n.pattern.exclude) != 0L)) {

      p <- p + ggplot2::labs(caption = paste0("Note. ", length(n.pattern.exclude), ifelse(length(n.pattern.exclude) == 1L, " pattern ", " patterns "), " with less than ", n.pattern, " cases removed.")) +
        ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0.5, vjust = 5))

    }

  #_____________________________________________________________________________
  #
  # Tests for Homoscedasticity in a Linear Model ———————————————————————————————
  }, test.homo = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    .plot.argument(geom = c("hline", "loess", "ci.loess"), hline.type = "solid", hline.width = 1L, hline.col = "#FE6100", point.shape = 21L, point.size = 2L, point.fill =  "#648FFF", point.col = "black", smooth.fill = "#56B4E9" , smooth.alpha = 0.15, smooth.type = "dashed", smooth.width = 0.8, smooth.col = "#0057B8", xbreaks = "ggplot2::waiver()", ybreaks = "ggplot2::waiver()", x = x, envir = environment())

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Plot Data ####

    plotdat <- data.frame(pred = predict(x$model), resid = rstudent(x$model))

    if (isTRUE(is.null(ylim))) { ylim <- max(c(max(plotdat$resid), abs(min(plotdat$resid)))) |> (\(p) c(-p - 0.1, p + 0.1))() }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Create ggplot ####

    p <- ggplot2::ggplot(plotdat, ggplot2::aes(pred, resid))

    # Add horizontal line
    if (isTRUE("hline" %in% geom)) { p <- p + ggplot2::geom_hline(yintercept = 0L, linetype = hline.type, linewidth = hline.width, color = hline.col) }

    p <- p + ggplot2::geom_point(size = point.size, shape = point.shape, fill = point.fill, color = point.col) +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(name = "Predicted Value", limits = xlim, breaks = xbreaks) +
      ggplot2::scale_y_continuous(name = "Studentized Residual", limits = ylim, breaks = ybreaks) +
      ggplot2::theme(axis.title = ggplot2::element_text(size = axis.title.size),
                     axis.text = ggplot2::element_text(size = axis.text.size))

    # Add smoothing curve
    if (isTRUE("loess" %in% geom)) { p <- p + ggplot2::geom_smooth(method = loess, fill = smooth.fill, alpha = smooth.alpha, se = ifelse(isTRUE("ci.loess" %in% geom), TRUE, FALSE), formula = "y ~ x", linetype = smooth.type, linewidth = smooth.width, color = smooth.col) }

  #_____________________________________________________________________________
  #
  # Levene's Test for Homogeneity of Variance ——————————————————————————————————

  }, test.levene = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    .plot.argument(geom = c("violin", "box"), violin.alpha = 0.3, violin.trim = FALSE, box.alpha = 0.2, box.width = 0.2, jitter.size = 1.25, jitter.width = 0.05, jitter.height = 0, jitter.alpha = 0.2, gray = FALSE, start = 0.9, end = 0.4, ylab = "y", ybreaks = "ggplot2::waiver()", x = x, envir = environment())

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Create ggplot ####

    p <- ggplot2::ggplot(x$data, ggplot2::aes(group, y, fill = group)) +
      ggplot2::scale_x_discrete(name = xlab) +
      ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = title, subtitle = subtitle) +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5), plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::guides(fill = "none")

    # Add violin plots
    if (isTRUE("violin" %in% geom)) { p <- p + ggplot2::geom_violin(alpha = violin.alpha, trim = violin.trim) }

    # Add boxplots
    if (isTRUE("box" %in% geom)) { p <- p + ggplot2::geom_boxplot(alpha = box.alpha, width = box.width) }

    # Add jittered points
    if (isTRUE("jitter" %in% geom)) { p <- p + ggplot2::geom_jitter(alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size) }

    # Gray color scales
    if (isTRUE(gray)) {

      p <- p + ggplot2::scale_fill_grey(start = start, end = end)

    # User-specified colors
    } else {

      if (isTRUE(!is.null(color))) { p <- p + ggplot2::scale_fill_manual(values = scale.fill.col) }

    }

  #_____________________________________________________________________________
  #
  # Tests for Linearity in a Linear Model, test.linear() ———————————————————————

  }, test.linear = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    .plot.argument(geom = c("lm", "loess", "ci.lm", "ci.loess"), point.shape = 21L, point.size = 2L, point.fill = "#648FFF", point.col = "black", smooth.fill = c("#FE6100", "#56B4E9"),
                   smooth.alpha = c(0.1, 0.15), smooth.type = c("solid", "dashed"), smooth.width = c(1L, 0.8), smooth.col = c("#FE6100", "#0057B8"), xlab = "Predictor", xbreaks = "ggplot2::waiver()",
                   ylab = "Partial Residual", ybreaks = "ggplot2::waiver()", x = x, envir = environment())

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Plot Data ####

    plotdat <- attr(terms(x$model), which = "term.labels") |> (\(p) data.frame(var = factor(rep(p, each = nrow(x$model$model)), levels = p), pred = unlist(x$model$model[, p]), resid = as.vector(residuals(x$model, type = "partial"))))()

    if (isTRUE(is.null(ylim))) { ylim <- max(c(max(plotdat$resid), abs(min(plotdat$resid)))) |> (\(p) c(-p - 0.1, p + 0.1))() }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Create ggplot ####

    p <- ggplot2::ggplot(plotdat, ggplot2::aes(pred, resid))

    # Add regression line
    if (isTRUE("lm" %in% geom)) { p <- p + ggplot2::geom_smooth(method = "lm", se = ifelse(isTRUE("ci.lm" %in% geom), TRUE, FALSE), formula = "y ~ x", linetype = smooth.type[1L], linewidth = smooth.width[1L], color = smooth.col[1L], fill = smooth.fill[1L], alpha = smooth.alpha[1L]) }

    # Add less line
    if (isTRUE("loess" %in% geom)) { p <- p + ggplot2::geom_smooth(method = "loess", se = ifelse(isTRUE("ci.loess" %in% geom), TRUE, FALSE), formula = "y ~ x", linetype =  smooth.type[2L], linewidth = smooth.width[2L], color = smooth.col[2L], fill = smooth.fill[2L], alpha = smooth.alpha[2L]) }

    p <- p + ggplot2::geom_point(size = point.size, shape = point.shape, fill = point.fill, color = point.col) +
      ggplot2::facet_wrap(~ var, scales = "free") +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(xlab, limits = xlim, breaks = xbreaks) +
      ggplot2::scale_y_continuous(ylab, limits = ylim, breaks = ybreaks) +
      ggplot2::theme(strip.text = ggplot2::element_text(size = strip.text.size),
                     axis.title = ggplot2::element_text(size = axis.title.size),
                     axis.text = ggplot2::element_text(size = axis.text.size))

  #_____________________________________________________________________________
  #
  # Tests for Univariate and Multivariate Normality, test.norm() ———————————————

  }, test.norm = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    .plot.argument(geom = c("qq", "hist"), ribbon.fill = "#56B4E9", ribbon.col = "#0057B8", ribbon.alpha = 0.1, abline.col = "#D55E00", abline.width = 0.7,
                   point.shape = 21L, point.size = 2L, point.fill = "#648FFF", point.col = "black", hist.col = "black", hist.fill = "gray90", function.type = c("solid", "dashed"),
                   density.width = 1L, density.type = "dashed", density.col = "#0057B8", function.width = c(1L, 1L), function.col = c("#D55E00", "#0057B8"),
                   xbreaks = "ggplot2::waiver()", ybreaks = "ggplot2::waiver()", x = x, envir = environment())

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Univariate: One Variable ####

    if (isTRUE(is.null(dim(x$data)))) {

      #—————————————————————————————————————— #
      ### Q-Q-Plot ####

      if (isTRUE("qq" %in% geom)) {

        #···················
        #### Default Argument Specification ####

        .plot.argument(xlab = "Theoretical Quantiles of the Normal Distribution", ylab = "Sample Quantiles", x = x, envir = environment())

        #···················
        #### ggplot ####

        p1 <- ggplot2::ggplot(.qqdat(x$data), ggplot2::aes(x = q, y = y)) +
          ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = ribbon.fill, color = ribbon.col, alpha = ribbon.alpha) +
          ggplot2::geom_abline(ggplot2::aes(intercept = intercept, slope = slope), color = abline.col, linewidth = abline.width) +
          ggplot2::geom_point(size = point.size, shape = point.shape, fill = point.fill, color = point.col) +
          ggplot2::theme_bw() +
          ggplot2::scale_x_continuous(name = xlab) +
          ggplot2::scale_y_continuous(name = ylab) +
          ggplot2::theme(axis.title = ggplot2::element_text(size = axis.title.size),
                         axis.text = ggplot2::element_text(size = axis.text.size))

      }

      #—————————————————————————————————————— #
      ### Histogram ####

      if (isTRUE("hist" %in% geom)) {

        #···················
        #### Default Argument Specification ####

        if (is.null(xlim)) { xlim <- max(c(abs(diff(c(mean(x$data), min(x$data)))), abs(diff(c(mean(x$data), max(x$data)))))) |> (\(p) c(mean(x$data) - p + 0.1, mean(x$data) + p + 0.1))() }

        #···················
        #### Create ggplot ####

        p2 <- ggplot2::ggplot(data.frame(x = x$data), ggplot2::aes(x)) +
          ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), binwidth = hist.binwidth, color = hist.col, fill = hist.fill) +
          ggplot2::stat_function(fun = dnorm, args = list(mean = mean(x$data, na.rm = TRUE), sd = sd(x$data, na.rm = TRUE)),
                                 col = function.col[1L], linetype = function.type[1L], linewidth = function.width[1L]) +
          ggplot2::geom_density(color = function.col[2L], linetype = function.type[2L], linewidth = function.width[2L]) +
          ggplot2::theme_bw() +
          ggplot2::scale_x_continuous("", limits = xlim, breaks = xbreaks) +
          ggplot2::scale_y_continuous("Density", limits = ylim, breaks = ybreaks, expand = ggplot2::expansion(mult = c(0, 0.05))) +
          ggplot2::theme(axis.title = ggplot2::element_text(size = axis.title.size),
                         axis.text = ggplot2::element_text(size = axis.text.size))

      }

      #···················
      #### Arrange Plots ####

      if (isTRUE(all(c("qq", "hist") %in% geom))) {

        p <- patchwork::wrap_plots(p1, p2, ncol = 2L)

      } else if (isTRUE(all("qq" %in% geom))) {

        p <- p1

      } else if (isTRUE(all("hist" %in% geom))) {

        p <- p2

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Univariate: Multiple Variables ####

    } else if (isTRUE(all(x$args$print == "uni"))) {

      # Default Argument Specification
      .plot.argument(facet.ncol = 3L, x = x, envir = environment())

      # Single imputation
      if (isTRUE(x$args$impute && any(is.na(x$data)))) {

        df <- mice::complete(mice::mice(x$data, m = 1L, method = "pmm", visitSequence = "monotone", seed = 42L, maxit = 30L, printFlag = FALSE))

      # Listwise deletion
      } else {

        complete.cases(x$data) |> (\(p) {

          assign("df", x$data[p, , drop = FALSE], envir = parent.frame())

          if (isTRUE(any(!p))) { warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", sum(!p)), call. = FALSE) }

        })()

      }

      # Plot data
      plotdat <- do.call("rbind", lapply(seq_along(df), function(y) { data.frame(variable = colnames(df)[y], .qqdat(df[, y])) }))

      # Factor
      plotdat$variable <- factor(plotdat$variable, levels = colnames(df))

      #—————————————————————————————————————— #
      ### Q-Q-Plot ####

      if (isTRUE(all(c("qq", "hist") %in% geom) || isTRUE(all("qq" %in% geom)))) {

        #···················
        #### Default Argument Specification ####

        .plot.argument(xlab = "Theoretical Quantiles of the Normal Distribution", ylab = "Sample Quantiles", facet.scales = "free_y", x = x, envir = environment())

        #···················
        #### Create ggplot ####

        p <- ggplot2::ggplot(plotdat, ggplot2::aes(x = q, y = y)) +
          ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = ribbon.fill, color = ribbon.col, alpha = ribbon.alpha) +
          ggplot2::geom_abline(ggplot2::aes(intercept = intercept, slope = slope), color = abline.col, linewidth = abline.width) +
          ggplot2::geom_point(size = point.size, shape = point.shape, fill = point.fill, color = point.col) +
          ggplot2::facet_wrap(~ variable, ncol = facet.ncol, nrow = facet.nrow, scales = facet.scales) +
          ggplot2::theme_bw() +
          ggplot2::scale_x_continuous(name = xlab) +
          ggplot2::scale_y_continuous(name = ylab) +
          ggplot2::theme(axis.title = ggplot2::element_text(size = axis.title.size),
                         axis.text = ggplot2::element_text(size = axis.text.size),
                         strip.text = ggplot2::element_text(size = strip.text.size))

      #—————————————————————————————————————— #
      ### Histogram ####

      } else if (isTRUE(all("hist" %in% geom))) {

        #···················
        #### Default Argument Specification ####

        .plot.argument(facet.scales = "free", x = x, envir = environment())

        #···················
        #### Create ggplot ####

        p <- ggplot2::ggplot(plotdat, ggplot2::aes(y)) +
          ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), binwidth = hist.binwidth, color = hist.col, fill = hist.fill) +
          ggplot2::geom_density(color = density.col, linetype = density.type, linewidth = density.width) +
          ggplot2::facet_wrap(~ variable, ncol = facet.ncol, nrow = facet.nrow, scales = facet.scales) +
          ggplot2::theme_bw() +
          ggplot2::scale_x_continuous(name = "", limits = xlim, breaks = xbreaks) +
          ggplot2::scale_y_continuous(name = "Density", limits = ylim, breaks = ybreaks, expand = ggplot2::expansion(mult = c(0, 0.05))) +
          ggplot2::theme(axis.title = ggplot2::element_text(size = axis.title.size),
                         axis.text = ggplot2::element_text(size = axis.text.size),
                         strip.text = ggplot2::element_text(size = strip.text.size))

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Multivariate ####

    } else {

      #···················
      #### Default Argument Specification ####

      .plot.argument(xlab = "Chi-Square Quantile", ylab = "Squared Mahalanobis Distance", x = x, envir = environment())

      #···················
      #### Data ####

      # Single imputation
      if (isTRUE(x$args$impute && any(is.na(x$data)))) {

        df <- as.matrix(mice::complete(mice::mice(x$data, m = 1L, method = "pmm", visitSequence = "monotone", seed = 42L, maxit = 30L, printFlag = FALSE)))

      # Listwise deletion
      } else {

        complete.cases(x$data) |> (\(p) {

          assign("df", as.matrix(x$data[p, , drop = FALSE]), envir = parent.frame())

          if (isTRUE(any(!p))) { warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", sum(!p)), call. = FALSE) }

        })()

      }

      # Sample size
      n <- nrow(df)

      # Distance and quantiles
      d <- scale(df, scale = FALSE) |> (\(p) diag(p %*% solve((((n - 1L) / n) * cov(df)), tol = 1e-25) %*% t(p)))()
      q <- qchisq((rank(d) - 0.5) / n, df = ncol(df))

      #···················
      #### Create ggplot ####

      p <- ggplot2::ggplot(data.frame(q = q, d = d), ggplot2::aes(x = q, y = d)) +
        ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1), color = abline.col, linewidth = abline.width) +
        ggplot2::geom_point(size = point.size, shape = point.shape, fill = point.fill, color = point.col) +
        ggplot2::theme_bw() +
        ggplot2::scale_x_continuous(name = xlab) +
        ggplot2::scale_y_continuous(name = ylab) +
        ggplot2::theme(axis.title = ggplot2::element_text(size = axis.title.size),
                       axis.text = ggplot2::element_text(size = axis.text.size))

    }

  #_____________________________________________________________________________
  #
  # t-Test —————————————————————————————————————————————————————————————————————

  }, test.t = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    .plot.argument(geom = c("bar", "point", "ci", "line", "jitter"), point.size = 3L, errorbar.width = 0.1, hline.type = "dotted", hline.width = 0.8, jitter.size = 1.25, jitter.width = 0.05, jitter.height = 0L, jitter.alpha = 0.1,
                   subtitle = "Confidence Interval", ylab = "y", ybreaks = "ggplot2::waiver()", facet.ncol = 3L, facet.scales = "free_y", x = x, envir = environment())

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## One-, Two-, or Paired Sample ####

    switch(x$sample,
           #—————————————————————————————————————— #
           ### One-Sample ####

           "one" = {

             #...................
             #### Create ggplot ####

             # Subtitle
             if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% Confidence Interval") }

             p <- ggplot2::ggplot(data.frame(x = x$data$x), ggplot2::aes(x = 0L, y = x)) +
               ggplot2::scale_x_continuous(name = xlab, limits = c(-2L, 2L)) +
               ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
               ggplot2::labs(title = title, subtitle = subtitle) +
               ggplot2::theme_bw() + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                                                    plot.subtitle = ggplot2::element_text(hjust = 0.5),
                                                    axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())

             #...................
             #### Bars ####

             if (isTRUE("bar" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_bar(data = x$result, ggplot2::aes(0, m), stat = "summary", fun = "mean")) }

             #...................
             #### Point ####

             if (isTRUE("point" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_point(data = x$result, ggplot2::aes(0, m), stat = "identity", size = point.size)) }

             #...................
             #### Confidence Intervals ####

             if (isTRUE("ci" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(data = x$result, ggplot2::aes(0, m, ymin = m.low, ymax = m.upp), width = errorbar.width)) }

             #...................
             #### Jittered Points ####

             if (isTRUE("jitter" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_jitter(data = data.frame(x = x$data$x), ggplot2::aes(x = 0L, y = x), alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

             #...................
             #### Horizontal Line ####

             if (isTRUE("line" %in% geom)) { p <- p + ggplot2::geom_hline(data = data.frame(mu = x$args$mu), ggplot2::aes(yintercept = mu), linetype = hline.type, linewidth = hline.width) }

           #—————————————————————————————————————— #
           ### Two-Sample ####

           }, "two" = {

             # Grouping variable
             group.var <- attr(terms(x$formula[-2L]), "term.labels")

             # Outcome variable
             y.var <- setdiff(all.vars(as.formula(x$formula)), attr(terms(x$formula[-2L]), "term.labels"))

             #···················
             #### One Outcome Variable ####

             if (isTRUE(length(y.var) == 1L)) {

               # Plot data
               plotdat <- split(x$data[, 1L], f = x$data[, 2L]) |> (\(y) data.frame(group = factor(c(rep(x$result[1L, "group"], times = length(y[[1L]])), rep(x$result[2L, "group"], times = length(y[[2L]])))), y = unlist(y)))()

               # Confidence interval
               ci.table <- misty::ci.mean(plotdat[, "y"], group = plotdat[, "group"], adjust = adjust, conf.level = conf.level, output = FALSE)$result

               #...................
               #### Create ggplot ####

               # Subtitle
               if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% ", ifelse(isTRUE(adjust), "Difference-Adjusted ", ""), "Confidence Interval") } else { subtitle }

               p <- ggplot2::ggplot(ci.table, ggplot2::aes(group, m)) +
                 ggplot2::scale_x_discrete(name = xlab) +
                 ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
                 ggplot2::labs(title = title, subtitle = subtitle) + ggplot2::theme_bw() +
                 ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5), plot.title = ggplot2::element_text(hjust = 0.5))

             #···················
             #### More Than One Outcome Variable ####

             } else {

               # Plot data
               plotdat <- do.call("rbind", lapply(y.var, function(y) split(x$data[, y], f = x$data[, group.var]) |> (\(p) data.frame(variable = y, group = factor(c(rep(x$result[1L, "group"], times = length(p[[1L]])), rep(x$result[2L, "group"], times = length(p[[2L]])))), y = unlist(p)) )()))

               # Confidence interval
               ci.table <- misty::ci.mean(x$data[, y.var], group = x$data[, group.var], sort.var = TRUE, adjust = adjust, conf.level = conf.level, output = FALSE)$result

               #...................
               #### Create ggplot ####

               # Subtitle
               if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% ", ifelse(isTRUE(adjust), "Difference-Adjusted ", ""), "Confidence Interval") } else { subtitle }

               p <- ggplot2::ggplot(ci.table, ggplot2::aes(group, m)) +
                 ggplot2::facet_wrap(~ variable, ncol = facet.ncol, nrow = facet.nrow, scales = facet.scales) +
                 ggplot2::scale_x_discrete(name = xlab) +
                 ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
                 ggplot2::labs(title = title, subtitle = subtitle) + ggplot2::theme_bw() +
                 ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5), plot.title = ggplot2::element_text(hjust = 0.5))

              }

             #...................
             #### Bars ####

             if (isTRUE("bar" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_bar(ggplot2::aes(group, m), stat = "summary", fun = "mean")) }

             #...................
             #### Point ####

             if (isTRUE("point" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_point(ggplot2::aes(group, m), stat = "identity", size = point.size)) }

             #...................
             #### Confidence Intervals ####

             if (isTRUE("ci" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(ggplot2::aes(group, m, ymin = low, ymax = upp), width = errorbar.width)) }

             #...................
             #### Jittered Points ####

             if (isTRUE("jitter" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_jitter(data = plotdat, ggplot2::aes(x = group, y = y), alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

           #—————————————————————————————————————— #
           ### Paired-Sample ####

           }, "paired" = {

             #...................
             #### Create ggplot ####

             # Subtitle
             if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% Confidence Interval") }

             p <- ggplot2::ggplot(data.frame(x = x$data$y - x$data$x), ggplot2::aes(x = 0L, y = x)) +
               ggplot2::scale_x_continuous(name = xlab, limits = c(-2L, 2L)) +
               ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
               ggplot2::labs(title = title, subtitle = subtitle) +
               ggplot2::theme_bw() + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                                                    plot.subtitle = ggplot2::element_text(hjust = 0.5),
                                                    axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())

             #...................
             #### Bars ####

             if (isTRUE("bar" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_bar(data = x$result, ggplot2::aes(0, m.diff), stat = "summary", fun = "mean")) }

             #...................
             #### Point ####

             if (isTRUE("point" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_point(data = x$result, ggplot2::aes(0, m.diff), stat = "identity", size = point.size)) }

             #...................
             #### Confidence Intervals ####

             if (isTRUE("ci" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(data = x$result, ggplot2::aes(0, m.diff, ymin = m.low, ymax = m.upp), width = errorbar.width)) }

             #...................
             #### Jittered Points ####

             if (isTRUE("jitter" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_jitter(data = data.frame(x = x$data$y - x$data$x), ggplot2::aes(x = 0L, y = x), alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

             #...................
             #### Horizontal Line ####

             if (isTRUE("line" %in% geom)) { p <- p + ggplot2::geom_hline(yintercept = 0L, linetype = hline.type, linewidth = hline.width) }

           })

  #_____________________________________________________________________________
  #
  # Welch's Test ———————————————————————————————————————————————————————————————

  }, test.welch = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    .plot.argument(geom = c("bar", "point", "ci", "jitter"), adjust = ".x", point.size = 3L, errorbar.width = 0.1, jitter.size = 1.25, jitter.width = 0.05, jitter.height = 0L, jitter.alpha = 0.1, subtitle = "Confidence Interval", ylab = "y", ybreaks = "ggplot2::waiver()", facet.ncol = 3L,  facet.scales = "free_y", x = x, envir = environment())

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Means and (Difference-Adjusted) Confidence Intervals by Group ####

    # Grouping variable
    group.var <- attr(terms(x$formula[-2L]), "term.labels")

    # Outcome variable
    y.var <- setdiff(all.vars(as.formula(x$formula)), attr(terms(x$formula[-2L]), "term.labels"))

    #—————————————————————————————————————— #
    ### One Outcome Variable ####

    if (isTRUE(length(y.var) == 1L)) {

      # Plot data
      plotdat <- split(x$data[, 1L], f = x$data[, 2L]) |> (\(p) data.frame(group = factor(unlist(sapply(names(p), function(y) rep(y, times = length(p[[y]]))))), y = unlist(p)))()

      # Confidence interval
      ci.table <- misty::ci.mean(plotdat[, "y"], group = plotdat[, "group"], adjust = adjust, conf.level = conf.level, output = FALSE)$result

      #...................
      #### Create ggplot ####

      # Subtitle
      if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% ", ifelse(isTRUE(adjust), "Difference-Adjusted ", ""), "Confidence Interval") } else { subtitle }

      p <- ggplot2::ggplot(ci.table, ggplot2::aes(group, m)) +
        ggplot2::scale_x_discrete(name = xlab) +
        ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
        ggplot2::labs(title = title, subtitle = subtitle) + ggplot2::theme_bw() +
        ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5), plot.title = ggplot2::element_text(hjust = 0.5))

    #—————————————————————————————————————— #
    ### More Than One Outcome Variable ####

    } else {

      # Plot data
      plotdat <- do.call("rbind", lapply(y.var, function(y) split(x$data[, y], f = x$data[, group.var]) |> (\(p) data.frame(variable = y, group = factor(c(rep(x$result[1L, "group"], times = length(p[[1L]])), rep(x$result[2L, "group"], times = length(p[[2L]])))), y = unlist(p)) )()))

      # Confidence interval
      ci.table <- misty::ci.mean(x$data[, y.var], group = x$data[, group.var], sort.var = TRUE, adjust = adjust, conf.level = conf.level, output = FALSE)$result

      #...................
      #### Create ggplot ####

      # Subtitle
      if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% ", ifelse(isTRUE(adjust), "Difference-Adjusted ", ""), "Confidence Interval") } else { subtitle }

      p <- ggplot2::ggplot(ci.table, ggplot2::aes(group, m)) +
        ggplot2::facet_wrap(~ variable, ncol = facet.ncol, nrow = facet.nrow, scales = facet.scales) +
        ggplot2::scale_x_discrete(name = xlab) +
        ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
        ggplot2::labs(title = title, subtitle = subtitle) + ggplot2::theme_bw() +
        ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5), plot.title = ggplot2::element_text(hjust = 0.5))

    }

    #—————————————————————————————————————— #
    ### Bars ####

    if (isTRUE("bar" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_bar(ggplot2::aes(group, m), stat = "summary", fun = "mean")) }

    #—————————————————————————————————————— #
    ### Point ####

    if (isTRUE("point" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_point(ggplot2::aes(group, m), stat = "identity", size = point.size)) }

    #—————————————————————————————————————— #
    ### Confidence Intervals ####

    if (isTRUE("ci" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(ggplot2::aes(group, m, ymin = low, ymax = upp), width = errorbar.width)) }

    #—————————————————————————————————————— #
    ### Jittered Points ####

    if (isTRUE("jitter" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_jitter(data = plotdat, ggplot2::aes(x = group, y = y), alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

  #_____________________________________________________________________________
  #
  # z-Test —————————————————————————————————————————————————————————————————————

  }, test.z = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    .plot.argument(geom = c("bar", "point", "ci", "line", "jitter"), point.size = 3L, errorbar.width = 0.1, hline.type = "dotted", hline.width = 0.8, jitter.size = 1.25, jitter.width = 0.05, jitter.height = 0L, jitter.alpha = 0.1, subtitle = "Confidence Interval", ylab = "y", ybreaks = "ggplot2::waiver()", facet.scales = "free_y", x = x, envir = environment())

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## One-, Two-, or Paired Sample ####

    switch(x$sample,
           #—————————————————————————————————————— #
           ### One-Sample ####

           "one" = {

             #···················
             #### Create ggplot ####

             # Subtitle
             if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% Confidence Interval") }

             p <- ggplot2::ggplot(data.frame(x = x$data$x), ggplot2::aes(x = 0L, y = x)) +
               ggplot2::scale_x_continuous(name = xlab, limits = c(-2L, 2L)) +
               ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
               ggplot2::labs(title = title, subtitle = subtitle) +
               ggplot2::theme_bw() + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                                                    plot.subtitle = ggplot2::element_text(hjust = 0.5),
                                                    axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())

             #···················
             #### Bars ####

             if (isTRUE("bar" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_bar(data = x$result, ggplot2::aes(0L, m), stat = "summary", fun = "mean")) }

             #···················
             #### Point ####

             if (isTRUE("point" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_point(data = x$result, ggplot2::aes(0L, m), stat = "identity", size = point.size)) }

             #...................
             #### Confidence Intervals ####

             if (isTRUE("ci" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(data = x$result, ggplot2::aes(0L, m, ymin = m.low, ymax = m.upp), width = errorbar.width)) }

             #...................
             #### Jittered Points ####

             if (isTRUE("hitter" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_jitter(data = data.frame(x = x$data$x), ggplot2::aes(x = 0L, y = x), alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

             #...................
             #### Horizontal Line ####

             if (isTRUE("line" %in% geom)) { p <- p + ggplot2::geom_hline(data = data.frame(mu = x$args$mu), ggplot2::aes(yintercept = mu), linetype = hline.type, linewidth = hline.width) }

           #—————————————————————————————————————— #
           ### Two-Sample ####

           }, "two" = {

             # Plot data
             plotdat <- split(x$data[, 1L], f = x$data[, 2L]) |> (\(y) data.frame(group = factor(c(rep(x$result[1L, "group"], times = length(y[[1L]])), rep(x$result[2L, "group"], times = length(y[[2L]])))), y = unlist(y)))()

             # Confidence interval
             ci.table <- misty::ci.mean(plotdat[, "y"], group = plotdat[, "group"], adjust = adjust, conf.level = conf.level, output = FALSE)$result

             #...................
             #### Create ggplot ####

             # Subtitle
             if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% ", ifelse(isTRUE(adjust), "Difference-Adjusted ", ""), "Confidence Interval") } else { subtitle }

             p <- ggplot2::ggplot(ci.table, ggplot2::aes(group, m)) +
               ggplot2::scale_x_discrete(name = xlab) +
               ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
               ggplot2::labs(title = title, subtitle = subtitle) + ggplot2::theme_bw() +
               ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5), plot.title = ggplot2::element_text(hjust = 0.5))

             #...................
             #### Bars ####

             if (isTRUE("bar" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_bar(ggplot2::aes(group, m), stat = "summary", fun = "mean")) }

             #...................
             #### Point ####

             if (isTRUE("point" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_point(ggplot2::aes(group, m), stat = "identity", size = point.size)) }

             #...................
             #### Confidence Intervals ####

             if (isTRUE("ci" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(ggplot2::aes(group, m, ymin = low, ymax = upp), width = errorbar.width)) }

             #...................
             #### Jittered Points ####

             if (isTRUE("jitter" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_jitter(data = plotdat, ggplot2::aes(x = group, y = y), alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

           #—————————————————————————————————————— #
           ### Paired-Sample ####

           }, "paired" = {

             #...................
             #### Create ggplot ####

             # Subtitle
             if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% Confidence Interval") }

             p <- ggplot2::ggplot(data.frame(x = x$data$y - x$data$x), ggplot2::aes(x = 0L, y = x)) +
               ggplot2::scale_x_continuous(name = xlab, limits = c(-2L, 2L)) +
               ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
               ggplot2::labs(title = title, subtitle = subtitle) +
               ggplot2::theme_bw() + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                                                    plot.subtitle = ggplot2::element_text(hjust = 0.5),
                                                    axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())

             #...................
             #### Bars ####

             if (isTRUE("bar" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_bar(data = x$result, ggplot2::aes(0L, m.diff), stat = "summary", fun = "mean")) }

             #...................
             #### Point ####

             if (isTRUE("point" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_point(data = x$result, ggplot2::aes(0L, m.diff), stat = "identity", size = point.size)) }

             #...................
             #### Confidence Intervals ####

             if (isTRUE("ci" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(data = x$result, ggplot2::aes(0L, m.diff, ymin = m.low, ymax = m.upp), width = errorbar.width)) }

             #...................
             #### Jittered Points ####

             if (isTRUE("jitter" %in% geom)) { p <- p + suppressWarnings(ggplot2::geom_jitter(data = data.frame(x = x$data$y - x$data$x), ggplot2::aes(x = 0L, y = x), alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

             #...................
             #### Horizontal Line ####

             if (isTRUE("line" %in% geom)) { p <- p + ggplot2::geom_hline(yintercept = 0L, linetype = hline.type, linewidth = hline.width) }

           })

  })

  #_____________________________________________________________________________
  #
  # Save Plot ——————————————————————————————————————————————————————————————————

  if (isTRUE(!is.null(filename))) { suppressWarnings(suppressMessages(ggplot2::ggsave(filename = filename, plot = p, width = width, height = height, units = units, dpi = dpi))) }

  #_____________________________________________________________________________
  #
  # Plot ———————————————————————————————————————————————————————————————————————

  return(p)

}

#_______________________________________________________________________________
