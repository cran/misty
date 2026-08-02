#' Plots misty.object object
#'
#' This function plots an \code{misty.object} object.
#'
#' @param x                  \code{misty.object} object.
#' @param plot               see 'Details'.
#' @param bar                see 'Details'.
#' @param box                see 'Details'.
#' @param violin             see 'Details'.
#' @param hist               see 'Details'.
#' @param point              see 'Details'.
#' @param line               see 'Details'.
#' @param ci                 see 'Details'.
#' @param conf.level         see 'Details'.
#' @param adjust             see 'Details'.
#' @param jitter             see 'Details'.
#' @param density            see 'Details'.
#' @param square             see 'Details'.
#' @param rotate             see 'Details'.
#' @param binwidth           see 'Details'.
#' @param bins               see 'Details'.
#' @param fill               see 'Details'.
#' @param hist.apha          see 'Details'.
#' @param tile.alpha         see 'Details'.
#' @param violin.alpha       see 'Details'.
#' @param violin.trim        see 'Details'.
#' @param box.width          see 'Details'.
#' @param box.alpha          see 'Details'.
#' @param linetype           see 'Details'.
#' @param linewidth          see 'Details'.
#' @param line.col           see 'Details'.
#' @param intercept          see 'Details'.
#' @param density.col        see 'Details'.
#' @param density.linewidth  see 'Details'.
#' @param density.linetype   see 'Details'.
#' @param point.size         see 'Details'.
#' @param point.linewidth    see 'Details'.
#' @param point.linetype     see 'Details'.
#' @param point.shape        see 'Details'.
#' @param point.col          see 'Details'.
#' @param ci.col             see 'Details'.
#' @param ci.linewidth       see 'Details'.
#' @param ci.linetype        see 'Details'.
#' @param errorbar.width     see 'Details'.
#' @param dodge.width        see 'Details'.
#' @param jitter.size        see 'Details'.
#' @param jitter.width       see 'Details'.
#' @param jitter.height      see 'Details'.
#' @param jitter.alpha       see 'Details'.
#' @param gray               see 'Details'.
#' @param start              see 'Details'.
#' @param end                see 'Details'.
#' @param color              see 'Details'.
#' @param xlab               see 'Details'.
#' @param ylab               see 'Details'.
#' @param xlim               see 'Details'.
#' @param ylim               see 'Details'.
#' @param xbreaks            see 'Details'.
#' @param ybreaks            see 'Details'.
#' @param axis.title.size    see 'Details'.
#' @param axis.text.size     see 'Details'.
#' @param strip.text.size    see 'Details'.
#' @param title              see 'Details'.
#' @param subtitle           see 'Details'.
#' @param group.col          see 'Details'.
#' @param plot.margin        see 'Details'.
#' @param legend.title       see 'Details'.
#' @param legend.position    see 'Details'.
#' @param legend.box.margin  see 'Details'.
#' @param legend.key.size    see 'Details'.
#' @param legend.text.size   see 'Details'.
#' @param facet.ncol         see 'Details'.
#' @param facet.nrow         see 'Details'.
#' @param facet.scales       see 'Details'.
#' @param filename           a character string indicating the \code{filename}
#'                           argument including the file extension in the \code{ggsave}
#'                           function. Note that one of \code{".eps"}, \code{".ps"},
#'                           \code{".tex"}, \code{".pdf"} (default), \code{".jpeg"},
#'                           \code{".tiff"}, \code{".png"}, \code{".bmp"},
#'                           \code{".svg"} or \code{".wmf"} needs to be specified
#'                           as file extension in the \code{file} argument.
#' @param width              a numeric value indicating the \code{width} argument
#'                           for the \code{ggsave} function.
#' @param height             a numeric value indicating the \code{height} argument
#'                           for the \code{ggsave} function.
#' @param units              a character string indicating the \code{units} argument
#'                           in the \code{ggsave} function. Note that one of
#'                           \code{"in"}, \code{"cm"}, \code{"mm"}, or \code{"px"}
#'                           needs to be specified.
#' @param dpi                a numeric value indicating the \code{dpi} argument
#'                           for the \code{ggsave} function.
#' @param check              logical: if \code{TRUE} (default), argument specification
#'                           is checked.
#' @param ...                further arguments passed to or from other methods.
#'
#' @details
#' \describe{
#' This function provides plotting arguments depending on the type of the output
#' object specified for the argument \code{x}:
#' \item{\strong{Output object from \code{aov.b}, \code{aov.w}, and \code{test.welch}}
#' Function}{The \code{plot} function for the misty object of type \code{"aov.b"},
#' \code{"aov.w"}, and \code{test.welch} has following plotting arguments:
#' \itemize{
#'    \item{\code{bar}}: logical: if \code{TRUE} (default), bars representing means
#'    for each groups are drawn. Note that this argument is only available for the
#'    misty object of type \code{"aov.b"} and \code{"test.welch"}.
#'    \item{\code{point}}: logical: if \code{TRUE}, points representing means for
#'    each groups are drawn.
#'    \item{\code{line}}:  logical: if \code{TRUE} (default), a line connecting
#'    means of each groups and lines connecting data points are drawn
#'    when \code{jitter = TRUE}. Note that this argument is only available for
#'    misty object of type \code{"aov.w"}.
#'    \item{\code{ci}}: logical: if \code{TRUE} (default), error bars representing
#'    confidence intervals are drawn.
#'    \item{\code{jitter}}: jittered data points are drawn. Note that subject-specific
#'    lines are also drawn for the \code{"aov.w"} function when \code{line = TRUE}.
#'    \item{\code{conf.level}}: a numeric value between 0 and 1 (default: 0.95)
#'    indicating the confidence level of the interval.
#'    \item{\code{adjust}}: logical: if \code{TRUE} (default), difference-adjustment
#'    for the confidence intervals in a two-sample design is applied.
#'    \item{\code{point.size}}: a numeric value indicating the \code{size} (default:
#'    \code{3}) aesthetic for the point representing the mean value.
#'    \item{\code{line.width}}: a numeric value (default: \code{0.5}) indicating
#'    the \code{linewidth} aesthetic for the line connecting means of each Groups.
#'    Note that this argument is only available for the \code{"aov.w"} function.
#'    \item{\code{errorbar.width}}: a numeric value (default: \code{0.1}) indicating
#'    the horizontal bar width of the error bar.
#'    \item{\code{jitter.size}}: a numeric value (default: \code{1.25}) indicating
#'    the \code{size} aesthetic for the jittered data points.
#'    \item{\code{jitter.width}}: a numeric value (default: \code{0.05}) indicating
#'    the amount of horizontal jitter.
#'    \item{\code{jitter.height}}: a numeric value (default: \code{0}) indicating
#'    the amount of vertical jitter.
#'    \item{\code{jitter.alpha}}: a numeric value between 0 and 1 (default: \code{0.1})
#'    for specifying the \code{alpha} argument in the \code{geom_jitter} function
#'    for controlling the opacity of the jittered data points.
#'    \item{\code{xlab}}: a character string (default: \code{NULL}) specifying the
#'    labels for the x-axis.
#'    \item{\code{ylab}}: a character string (default: \code{"y"}) specifying the
#'    labels for the y-axis.
#'    \item{\code{ylim}}: a numeric vector of length two (default: \code{NULL})
#'    specifying limits of the limits of the y-axis.
#'    \item{\code{ybreaks}}: a numeric vector (default: \code{waiver()}) specifying
#'    the points at which tick-marks are drawn at the y-axis.
#'    \item{\code{title}}: a character string (default: \code{""}) specifying the
#'    text for the title for the plot.
#'    \item{\code{subtitle}}: a character string (default: \code{"Two-Sided Confidence Interval"}
#'    when \code{adjust = FALSE} or \code{"Two-Sided Difference-Adjusted Confidence Interval"}
#'    for the \code{aov.b} function and
#'    \code{"Two-Sided Difference-Adjusted Cousineau-Morey Confidence Interval Confidence Interval"}
#'    for the \code{aov.w} function when \code{adjust = TRUE}) specifying the text
#'    for the subtitle for the plot.
#'    \item{\code{filename}}:  character string indicating the \code{filename}
#'    argument including the file extension in the \code{ggsave} function.
#'    \item{\code{width}}: a numeric value indicating the \code{width} argument
#'    for the \code{ggsave} function.
#'    \item{\code{height}}: a numeric value indicating the \code{height} argument
#'    for the \code{ggsave} function.
#'    \item{\code{dpi}}: a numeric value indicating the \code{dpi} argument for
#'    the \code{ggsave} function.
#'    \item{\code{units}}: a character string (default: \code{"in"}) indicating
#'    the \code{units} argument  (default: \code{in}) for the \code{ggsave}
#'    function.
#' }
#' }
#' \item{\strong{Output object from \code{ci.*} Functions}}{The \code{plot}
#' function for the misty object of type \code{"ci.cor"}, \code{"ci.mean"},
#' \code{"ci.median"}, \code{"ci.prop"}, \code{"ci.var"}, and \code{"ci.sd"} has
#' following plotting arguments:
#' \itemize{
#'    \item{\code{plot}}: a character string indicating the type of the plot
#'    to display, i.e., \code{"ci"} (default) for displaying confidence intervals
#'    or \code{"boot"} for displaying bootstrap samples with histograms and density
#'    curves when the argument.
#'    \item{\code{hist}}: logical: if \code{TRUE} (default), histograms are
#'    drawn when \code{plot = "boot"}.
#'    \item{\code{density}}: logical: if \code{TRUE} (default), density curves are
#'    drawn when \code{plot = "boot"}.
#'    \item{\code{point}}: logical: if \code{TRUE} (default), vertical lines
#'    representing the point estimate are drawn when \code{plot = "boot"}.
#'    \item{\code{ci}}: logical: if \code{TRUE} (default), vertical lines
#'    representing the bootstrap confidence intervals are drawn when \code{plot = "boot"}.
#'    \item{\code{line}}: logical: if \code{TRUE} (default), a horizontal line
#'    is drawn when \code{plot = "ci"} or a vertical line is drawn when
#'    \code{plot = "boot"}.
#'    \item{\code{point.size}}: a numeric value (default: \code{2.5}) indicating
#'    the \code{size} argument in the \code{geom_point} function for controlling
#'    the size of points when plotting confidence intervals (\code{plot = "ci"}).
#'    \item{\code{point.shape}}: a numeric value between 0 and 25 (default: \code{19})
#'    or a character string as plotting symbol indicating the \code{shape} argument
#'    in the \code{geom_point} function for controlling the symbols of points
#'    when plotting confidence intervals (\code{plot = "ci"}).
#'    \item{\code{errorbar.width}}: a numeric value (default: \code{0.3}) indicating
#'    the \code{width} argument in the \code{geom_errorbar} function for controlling
#'    the width of the whiskers in the \code{geom_errorbar} function when plotting
#'    confidence intervals (\code{plot = "ci"}).
#'    \item{\code{dodge.width}}: a numeric value (default: \code{0.5}) indicating
#'    the \code{width} argument controlling the width of the \code{geom} elements
#'    to be dodged when specifying a grouping variable using the argument \code{group}
#'    when plotting confidence intervals (\code{plot = "ci"}).
#'    \item{\code{binwidth}}: a numeric value or a function (default: \code{NULL})
#'    for specifying the \item{\code{bins}}: a numeric value for specifying the
#'    \code{bins} argument in the \code{geom_histogram} function for controlling
#'    the number of bins when plotting bootstrap samples (\code{plot = "boot"}).
#'    \item{\code{hist.alpha}}: a numeric value between 0 and 1 (default: \code{0.4})
#'    for specifying the \code{alpha} argument in the \code{geom_histogram}
#'    function for controlling the opacity of the bars when plotting bootstrap
#'    samples (\code{plot = "boot"}).
#'    \item{\code{fill}}: a character string (default: \code{"gray85"}) specifying
#'    the \code{fill} argument in the \code{geom_histogram} function controlling
#'    the fill aesthetic when plotting bootstrap samples (\code{plot = "boot"}).
#'    Note that this argument applied only when no grouping variable was specified
#'    \code{group = NULL}.
#'    \item{\code{density.col}}: a character string (default: \code{"#0072B2"})
#'    specifying the \code{color} argument in the \code{geom_density} function
#'    controlling the color of the density curves when plotting bootstrap samples
#'    (\code{plot = "boot"}). Note that this argument applied only when no grouping
#'    variable was specified \code{group = NULL}.
#'    \item{\code{density.linewidth}}: a numeric value (default: \code{0.5}) specifying
#'    the \code{linewidth} argument in the \code{geom_density} function controlling
#'    the line width of the density curves when plotting bootstrap samples
#'    (\code{plot = "boot"}).
#'    \item{\code{density.linetype}}: a numeric value or character string (default:
#'    \code{0.5}) specifying the \code{linetype} argument in the \code{geom_density}
#'    function controlling the line type of the density curves when plotting
#'    bootstrap samples (\code{plot = "boot"}).
#'    \item{\code{point.col}}: a character string (default: \code{"#CC79A7"}) specifying
#'    the \code{color} argument in the \code{geom_vline} function for controlling
#'    the color of the vertical line displaying the point estimate when plotting
#'    bootstrap samples (\code{plot = "boot"}). Note that this argument applied
#'    only when no grouping variable was specified \code{group = NULL}.
#'    \item{\code{point.linewidth}}: a numeric value (default: \code{0.6}) specifying
#'    the \code{linewdith} argument in the \code{geom_vline} function for
#'    controlling the line width of the vertical line displaying the point estimate
#'    when plotting bootstrap samples (\code{plot = "boot"}).
#'    \item{\code{point.linetype}}: a numeric value or character string (default:
#'    \code{"solid"}) specifying the \code{linetype} argument in the \code{geom_vline}
#'    function controlling the line type of the vertical line displaying the
#'    point estimate when plotting bootstrap samples (\code{plot = "boot"}).
#'    \item{\code{ci.col}}: character string (default: \code{"black"}) specifying the
#'    \code{color} argument in the \code{geom_vline} function for controlling the
#'    color of the vertical line displaying bootstrap confidence intervals when
#'    plotting bootstrap samples (\code{plot = "boot"}). Note that this argument
#'    applied only when no grouping variable was specified \code{group = NULL}.
#'    \item{\code{ci.linewidth}}: a numeric value (default: \code{0.6}) specifying
#'    the \code{linewdith} argument in the \code{geom_vline} function for controlling
#'    the line width of the vertical line displaying bootstrap confidence intervals
#'    when plotting bootstrap samples (\code{plot = "boot"}).
#'    \item{\code{ci.linetype}}: a numeric value or character string (default:
#'    \code{"dashed"}) specifying the \code{linetype} argument in the \code{geom_vline}
#'    function controlling the line type of the vertical line displaying bootstrap
#'    confidence intervals when plotting bootstrap samples (\code{plot = "boot"}).
#'    \item{\code{intercept}}: a numeric value (default = \code{0}) indicating the
#'    \code{yintercept} or \code{xintercept} argument in the \code{geom_hline}
#'    or \code{geom_vline} function controlling the position of the horizontal
#'    or vertical line when \code{plot = "ci"} and \code{line = TRUE} or when
#'    \code{plot = "boot"} and \code{line = TRUE}.
#'    \item{\code{linetype}}: a character string (default: \code{"solid"}) indicating
#'    the \code{linetype} argument in the \code{geom_hline} or \code{geom_vline}
#'    function controlling the line type of the horizontal or vertical line
#'    \item{\code{line.col}}: a character string (default: \code{"gray65"}) indicating
#'    the \code{color} argument in the \code{geom_hline} or \code{geom_vline}
#'    function for controlling the color of the horizontal or vertical line.
#'    \item{\code{xlab}}: a character string indicating the \code{name} argument
#'    in the \code{scale_x_continuous} function for labeling the x-axis. The default
#'    setting is \code{xlab = NULL} when \code{plot = "ci"} and
#'    \code{xlab = "Correlation Coefficient"}, \code{xlab = "Arithmetic Mean"},
#'    \code{xlab = "Median"}, \code{xlab = "Proportion"}, \code{xlab = "Variance"},
#'    or \code{xlab = "Standard Deviation"}.
#'    \item{\code{ylab}}: a character string indicating the \code{name} argument
#'    in the \code{scale_y_continuous} function for labeling the y-axis. The
#'    default setting is \code{ylab = "Correlation Coefficient"}, \code{ylab = "Arithmetic Mean"},
#'    \code{ylab = "Median"}, \code{ylab = "Proportion"}, \code{ylab = "Variance"},
#'    or \code{ylab = "Standard Deviation"} when \code{plot = "ci"} and
#'    \code{ylab = "Probability Density f(x)"} when \code{plot = "boot"}.
#'    \item{\code{xlim}}: a numeric vector with two elements indicating the
#'    \code{limits} argument in the \code{scale_x_continuous} function for controlling
#'    the scale range of the x-axis. The default setting is \code{xlim = NULL}
#'    when \code{plot = "ci"} and \code{xlim = c(-1, 1)} for the correlation
#'    coefficient and proportion or \code{xlim = NULL) for the arithmetic mean,
#'    median, variance and standard deviation when \code{plot = "boot"}.
#'    \item{\code{ylim}}: a numeric vector with two elements indicating the
#'    \code{limits} argument in the \code{scale_y_continuous} function for controlling
#'    the scale range of the y-axis. The default setting is \code{ylim = c(-1, 1)}
#'    fpr the correlation coefficient and proportion and \code{ylim = NULL} for
#'    the arithmetic mean, median, variance and standard deviation when \code{plot = "ci"}
#'    and \code{xlim = NULL} when \code{plot = "boot"}.
#'    \item{\code{xbreaks}}: a numeric vector (default: \code{waiver()) indicating
#'    the \code{breaks} argument in the \code{scale_x_continuous} function for
#'    controlling the x-axis breaks.
#'    \item{\code{ybreaks}}:  a numeric vector (default: \code{waiver()) indicating
#'    the \code{breaks} argument in the \code{scale_y_continuous} function for
#'    controlling the y-axis breaks.
#'    \item{\code{axis.title.size}}: a numeric value (default: \code{11}) indicating
#'    the \code{size} argument in the \code{element_text} function for specifying
#'    the function controlling the font size of the axis title, i.e.
#'    \code{theme(axis.title = element_text(size = axis.text.size))}
#'    \item{\code{axis.text.size}}: a numeric value (default: 10) indicating the
#'    \code{size} argument in the \code{element_text} function for specifying the
#'    function controlling the font size of the axis text,
#'    i.e. \code{theme(axis.text = element_text(size = axis.text.size))}.
#'    \item{\code{strip.text.size}}: a numeric value (default: \code{11}) indicating
#'    the \code{size} argument in the \code{element_text} function for specifying
#'    the function controlling the font size of the strip text, i.e.
#'    \code{theme(strip.text = element_text(size = strip.text.size))}.
#'    \item{\code{title}}: a character string (default: \code{NULL}) indicating the
#'    \code{title} argument in the \code{labs} function for the subtitle of the
#'    plot.
#'    \item{\code{subtitle}}: a character string (default: \code{NULL}) indicating
#'    the \code{subtite} argument in the \code{labs} function for the subtitle of
#'    the plot.
#'    \item{\code{group.col}}: a character vector (default: \code{NULL}) indicating
#'    the \code{color} argument in the \code{scale_color_manual} and \code{scale_fill_manual}
#'    functions when specifying a grouping variable using the argument \code{group}.
#'    \item{\code{plot.margin}}: a numeric vector (default: \code{NA}) with four
#'    elements indicating the \code{plot.margin} argument in the \code{theme} function
#'    controlling the plot margins. The default setting is \code{c(5.5, 5.5, 5.5, 5.5)}
#'    but switches to \code{c(5.5, 5.5, -2.5, 5.5)} when specifying a grouping
#'    variable using the argument \code{group}.
#'    \item{\code{legend.title}}: a character string (default: \code{""}) indicating
#'    the \code{color} argument in the \code{labs} function for specifying the
#'    legend title when specifying a grouping variable using the argument \code{group}.
#'    \item{\code{legend.position}}: a character string (default: \code{"bottom"})
#'    indicating the \code{legend.position} in the \code{theme} argument for
#'    controlling the position of the legend  function when specifying a
#'    grouping variable using the argument \code{group}.
#'    \item{\code{legend.box.margin}}: a numeric vector (default: \code{c(-10, 0, 0, 0)})
#'    with four elements indicating the \code{legend.box.margin} argument in the
#'    \code{theme} function for controlling the margins around the full legend
#'    area when specifying a grouping variable using the argument \code{group}.
#'    \item{\code{facet.ncol}}: a numeric value (default: \code{NULL}) indicating the
#'    \code{ncol} argument in the \code{facet_wrap} function for controlling the
#'    number of columns when specifying a split variable using the argument \code{split}.
#'    \item{\code{facet.nrow}}: a numeric value (default: \code{NULL}) indicating the
#'    \code{nrow} argument in the \code{facet_wrap} function for controlling the
#'    number of rows when specifying a split variable using the argument \code{split}.
#'    \item{\code{facet.scales}}: a character string (default: \code{"free_y"}) indicating
#'    the \code{scales} argument in the \code{facet_wrap} function for controlling the
#'    scales shared across facets i.e. \code{"fixed"}, \code{"free_x"},
#'    \code{"free_y"} (default) or \code{"free"} when specifying a split variable
#'    using the argument \code{split}.
#'    \item{\code{filename}}:  character string indicating the \code{filename}
#'    argument including the file extension in the \code{ggsave} function.
#'    \item{\code{width}}: a numeric value indicating the \code{width} argument
#'    for the \code{ggsave} function.
#'    \item{\code{height}}: a numeric value indicating the \code{height} argument
#'    for the \code{ggsave} function.
#'    \item{\code{dpi}}: a numeric value indicating the \code{dpi} argument for
#'    the \code{ggsave} function.
#'    \item{\code{units}}: a character string (default: \code{"in"}) indicating
#'    the \code{units} argument  (default: \code{in}) for the \code{ggsave}
#'    function.
#' }
#' }
#' \item{\strong{Output object from \code{test.levene} Function}}{The \code{plot}
#' function for the misty object of type \code{"test.levene"} has following plotting
#' arguments:
#' \itemize{
#'    \item{\code{violin.alpha}}: a numeric value between 0 and 1 (default: \code{0.3})
#'    for specifying the \code{alpha} argument in the \code{geom_violin} function
#'    for controlling the opacity of the violins.
#'    \item{\code{violin.trim}}: logical: if \code{TRUE} (default: \code{FALSE}),
#'    the tails of the violins to the range of the data is trimmed.
#'    \item{\code{box.alpha}}: a numeric value between 0 and 1 (default: \code{0.2})
#'    for specifying the \code{alpha} argument in the \code{geom_boxplot} function
#'    for controlling the opacity of the boxplots.
#'    \item{\code{box.width}}: a numeric value between 0 and 1 (default: \code{0.2})
#'    for specifying the \code{alpha} argument in the \code{geom_boxplot} function
#'    for controlling the opacity of the boxplots.
#'    \item{\code{jitter.size}}: a numeric value (default: \code{1.25}) indicating
#'    the \code{size} aesthetic for the jittered data points.
#'    \item{\code{jitter.width}}: a numeric value (default: \code{0.05}) indicating
#'    the  amount of horizontal jitter.
#'    \item{\code{jitter.height}}: a numeric value (default: \code{0}) indicating
#'    the amount of vertical jitter.
#'    \item{\code{jitter.alpha}}: a numeric value between 0 and 1 (default: \code{0.2})
#'    for specifying the \code{alpha} argument in the \code{geom_jitter} function
#'    for controlling the opacity of the jittered data points.
#'    \item{\code{start}}: a numeric value between 0 and 1 (default: \code{0.9}),
#'    graphical  parameter to specify the gray value at the low end of the palette.
#'    \item{\code{end}}: a numeric value between 0 and 1 (default: \code{0.4}),
#'    graphical  parameter to specify the gray value at the high end of the palette.
#'    \item{\code{color}}: a character vector (default: \code{NULL}), indicating
#'    the color of the violins and the boxes. By default, default ggplot2 colors
#'    are used.
#'    \item{\code{xlab}}: a character string (default: \code{NULL}) specifying
#'    the labels for the x-axis.
#'    \item{\code{ylab}}: a character string (default: \code{NULL}) specifying
#'    the labels for the y-axis.
#'    \item{\code{ylim}}: a numeric vector (default: \code{NULL}) of length two specifying
#'    limits of the limits of the y-axis.
#'    \item{\code{ybreaks}}: a numeric vector (default: \code{waiver()})
#'    specifying the points at which tick-marks are drawn at the y-axis.
#'    \item{\code{title}}: a character string (default: \code{""}) specifying the
#'    text for the title for the plot.
#'    \item{\code{subtitle}}: a character string (default: \code{""}) specifying
#'    the text for the subtitle for the plot.
#'    \item{\code{filename}}:  character string indicating the \code{filename}
#'    argument including the file extension in the \code{ggsave} function.
#'    \item{\code{width}}: a numeric value indicating the \code{width} argument
#'    for the \code{ggsave} function.
#'    \item{\code{height}}: a numeric value indicating the \code{height} argument
#'    for the \code{ggsave} function.
#'    \item{\code{dpi}}: a numeric value indicating the \code{dpi} argument for
#'    the \code{ggsave} function.
#'    \item{\code{units}}: a character string (default: \code{"in"}) indicating
#'    the \code{units} argument  (default: \code{in}) for the \code{ggsave}
#'    function.
#' }
#' }
#' \item{\strong{Output object from \code{test.t} and \code{test.z}}}{The \code{plot}
#' function for the misty object of type \code{"test.t"} and \code{"test.z"} has
#' following plotting arguments:
#' \itemize{
#'    \item{\code{bar}}: logical: if \code{TRUE} (default), bars representing means
#'    for each groups are drawn.
#'    \item{\code{point}}: logical: if \code{TRUE}, points representing means for
#'    each groups are drawn.
#'    \item{\code{ci}}: logical: if \code{TRUE} (default), error bars representing
#'    confidence intervals are drawn.
#'    \item{\code{jitter}}: logical: if \code{TRUE}, jittered data points are drawn.
#'    \item{\code{line}}: logical: if \code{TRUE} (default), a horizontal line is
#'    drawn at \code{mu} for the one-sample t- or z-test or at 0 for the paired-sample
#'    t- or z-test.
#'    \item{\code{conf.level}}: a numeric value between 0 and 1 (default: 0.95)
#'    indicating the confidence level of the interval.
#'    \item{\code{adjust}}: logical: if \code{TRUE} (default), difference-adjustment
#'    for the confidence intervals in a two-sample design is applied.
#'    \item{\code{point.size}}: a numeric value indicating the \code{size} (default:
#'    \code{3}) aesthetic for the point representing the mean value.
#'    \item{\code{errorbar.width}}: a numeric value (default: \code{0.1}) indicating
#'    the horizontal bar width of the error bar.
#'    \item{\code{linetype}}: an integer value or character string (default: \code{3})
#'    specifying the line type for the line representing the population mean under
#'    the null hypothesis, i.e., 0 = blank, 1 = solid, 2 = dashed, 3 = dotted,
#'    4 = dotdash, 5 = longdash, or 6 = twodash.
#'    \item{\code{linewidth}}: a numeric value indicating the \code{linewidth}
#'    (default: \code{0.8} aesthetic for the line representing the population mean
#'    under the null hypothesis.
#'    \item{\code{jitter.size}}: a numeric value (default: \code{1.25}) indicating
#'    the \code{size} aesthetic for the jittered data points.
#'    \item{\code{jitter.width}}: a numeric value (default: \code{0.05}) indicating
#'    the amount of horizontal jitter.
#'    \item{\code{jitter.height}}: a numeric value (default: \code{0}) indicating
#'    the amount of vertical jitter.
#'    \item{\code{jitter.alpha}}: a numeric value between 0 and 1 (default: \code{0.1})
#'    for specifying the \code{alpha} argument in the \code{geom_jitter} function
#'    for controlling the opacity of the jittered data points.
#'    \item{\code{xlab}}: a character string (default: \code{NULL}) specifying the
#'    labels for the x-axis.
#'    \item{\code{ylab}}: a character string (default: \code{"y"}) specifying the
#'    labels for the y-axis.
#'    \item{\code{ylim}}: a numeric vector of length two (default: \code{NULL})
#'    specifying limits of the limits of the y-axis.
#'    \item{\code{ybreaks}}: a numeric vector (default: \code{waiver()}) specifying
#'    the points at which tick-marks are drawn at the y-axis.
#'    \item{\code{title}}: a character string (default: \code{""}) specifying the
#'    text for the title for the plot.
#'    \item{\code{subtitle}}: a character string (default: \code{"Two-Sided Confidence Interval"}
#'    when \code{adjust = FALSE} or \code{"Two-Sided Difference-Adjusted Confidence Interval"}
#'    when \code{adjust = TRUE}) specifying the text for the subtitle for the plot.
#'    \item{\code{filename}}:  character string indicating the \code{filename}
#'    argument including the file extension in the \code{ggsave} function.
#'    \item{\code{width}}: a numeric value indicating the \code{width} argument
#'    for the \code{ggsave} function.
#'    \item{\code{height}}: a numeric value indicating the \code{height} argument
#'    for the \code{ggsave} function.
#'    \item{\code{dpi}}: a numeric value indicating the \code{dpi} argument for
#'    the \code{ggsave} function.
#'    \item{\code{units}}: a character string (default: \code{"in"}) indicating
#'    the \code{units} argument  (default: \code{in}) for the \code{ggsave}
#'    function.
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
plot.misty.object <- function(x, plot = x$args$plot, bar = x$args$bar,
                              box = x$args$box, violin = x$args$violin,
                              hist = x$args$hist,point = x$args$point,
                              line = x$args$line, ci = x$args$ci,
                              conf.level = x$args$conf.level, adjust = x$args$adjust,
                              jitter = x$args$jitter, density = x$args$density,
                              square = x$args$square, rotate = x$args$rotate,
                              binwidth = x$args$binwidth, bins = x$args$bins,
                              fill = x$args$fill, hist.alpha = x$args$hist.alpha,
                              tile.alpha = x$args$tile.alpha,  violin.alpha = x$args$violin.alpha,
                              violin.trim = x$args$violin.trim, box.width = x$args$box.width,
                              box.alpha = x$args$box.alpha, linetype = x$args$linetype,
                              linewidth = x$args$linewidth, line.col = x$args$line.col,
                              intercept = x$args$intercept, density.col = x$args$density.col,
                              density.linewidth = x$args$density.linewidth,
                              density.linetype = x$args$density.linetype,
                              point.size = x$args$point.size,
                              point.linewidth = x$args$point.linewidth,
                              point.linetype = x$args$point.linetype,
                              point.shape = x$args$point.shape,
                              point.col = x$args$point.col, ci.col = x$args$ci.col,
                              ci.linewidth = x$args$ci.linewidth, ci.linetype = x$args$ci.linetype,
                              errorbar.width = x$args$errorbar.width, dodge.width = x$args$dodge.width,
                              jitter.size = x$args$jitter.size, jitter.width = x$args$jitter.width,
                              jitter.height = x$args$jitter.height, jitter.alpha = x$args$jitter.alpha,
                              gray = x$args$gray, start = x$args$start, end = x$args$end,
                              color = x$args$color, xlab = x$args$xlab, ylab = x$args$ylab,
                              xlim = x$args$xlim, ylim = x$args$ylim, xbreaks = x$args$xbreaks,
                              ybreaks = x$args$ybreaks, axis.title.size = x$args$axis.title.sizes,
                              axis.text.size = x$args$axis.text.size, strip.text.size = x$args$strip.text.size,
                              title = x$args$title, subtitle = x$args$subtitle,
                              group.col = x$args$group.col, plot.margin = x$args$plot.margin,
                              legend.title = x$args$legend.title, legend.position = x$args$legend.position,
                              legend.box.margin = x$args$legend.box.margin, legend.key.size = x$args$legend.key.size,
                              legend.text.size = x$args$legend.text.size, facet.ncol = x$args$facet.ncol,
                              facet.nrow = x$args$facet.nrow, facet.scales = x$args$facet.scales,
                              filename = x$args$filename, width = x$args$width, height = x$args$height,
                              units = x$args$units, dpi = x$args$dpi, check = TRUE, ...) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing or NULL
  if (isTRUE(missing(x) || is.null(x))) { stop("Please specify a misty object for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is a misty object
  if (isTRUE(!inherits(x, what = "misty.object"))) { stop("Please specify a misty object for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is supported by the function
  if (isTRUE(!x$type %in% c("aov.b", "aov.w", "boot.bs", "ci.cor", "ci.mean", "ci.median", "ci.prop", "ci.var", "ci.sd", "item.dfi", "multilevel.r2", "multilevel.r2.manual", "na.pattern", "test.levene", "test.welch", "test.z", "test.t"))) { stop("This type of misty object is not supported by the plot.misty.object() function.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check-----------------------------------------------------------------

  # Global Variables
  group <- id <- low <- m <- m.diff <- m.low <- m.upp <- mu <- n.pattern <- obs_miss <- part <- pattern <- upp <- variable <- NULL

  #_____________________________________________________________________________
  #
  # Between-Subject Analysis of Variance (ANOVA) -------------------------------
  switch(x$type, "aov.b" = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    if (is.null(point.size)) { point.size <- 3 }
    if (is.null(errorbar.width)) { errorbar.width <- 0.1 }
    if (is.null(jitter.size)) { jitter.size <- 1.25 }
    if (is.null(jitter.width)) { jitter.width <- 0.05 }
    if (is.null(jitter.height)) { jitter.height <- 0 }
    if (is.null(jitter.alpha)) { jitter.alpha <- 0.1 }
    if (is.null(jitter.height)) { jitter.height <- 0 }
    if (is.null(jitter.alpha)) { jitter.alpha <- 0.2 }
    if (is.null(subtitle)) { subtitle <- "Confidence Interval" }
    if (is.null(ylab)) { ylab <- "y" }
    if (is.null(ybreaks)) { ybreaks <- ggplot2::waiver() }
    if (is.null(units)) { units <- "in" }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Means and (Difference-Adjusted) Confidence Intervals by Group ####

    ci.table <- misty::ci.mean(x$data, y, group = "group", adjust = adjust, conf.level = conf.level, output = FALSE)$result

    #—————————————————————————————————————— # #
    ### Create ggplot ####

    # Subtitle
    if (isTRUE(ci)) { if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% ", ifelse(isTRUE(adjust), "Difference-Adjusted ", ""), "Confidence Interval") } else { subtitle } } else { subtitle <- "" }

    p <- ggplot2::ggplot(x$data, ggplot2::aes(group, y)) +
      ggplot2::scale_x_discrete(name = xlab) +
      ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = title, subtitle = subtitle) +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5), plot.title = ggplot2::element_text(hjust = 0.5))

    #—————————————————————————————————————— # #
    ### Bars ####

    if (isTRUE(bar)) { p <- p + suppressWarnings(ggplot2::geom_bar(data = ci.table, ggplot2::aes(group, m), stat = "summary", fun = "mean")) }

    #—————————————————————————————————————— # #
    ### Confidence Intervals ####

    if (isTRUE(ci)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(data = ci.table, ggplot2::aes(group, m, ymin = low, ymax = upp), width = errorbar.width)) }

    #—————————————————————————————————————— # #
    ### Points ####

    if (isTRUE(point)) { p <- p + suppressWarnings(ggplot2::geom_point(data = ci.table, ggplot2::aes(group, m), stat = "identity", size = point.size)) }

    #—————————————————————————————————————— # #
    ### Jittered Points ####

    if (isTRUE(jitter)) { p <- p + suppressWarnings(ggplot2::geom_jitter(alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

  #_____________________________________________________________________________
  #
  # Repeated Measures Analysis of Variance (Within-Subject ANOVA ---------------
  }, "aov.w" = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    if (is.null(point.size)) { point.size <- 3 }
    if (is.null(linewidth)) { linewidth <- 0.5 }
    if (is.null(errorbar.width)) { errorbar.width <- 0.1 }
    if (is.null(jitter.size)) { jitter.size <- 1.25 }
    if (is.null(jitter.width)) { jitter.width <- 0.05 }
    if (is.null(jitter.height)) { jitter.height <- 0 }
    if (is.null(jitter.alpha)) { jitter.alpha <- 0.1 }
    if (is.null(jitter.height)) { jitter.height <- 0 }
    if (is.null(jitter.alpha)) { jitter.alpha <- 0.2 }
    if (is.null(subtitle)) { subtitle <- "Confidence Interval" }
    if (is.null(ylab)) { ylab <- "y" }
    if (is.null(ybreaks)) { ybreaks <- ggplot2::waiver() }
    if (is.null(units)) { units <- "in" }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Compute Means and (Difference-Adjusted) Confidence Intervals by Group ####

    ci.table <- misty::ci.mean.w(x$data$wide, adjust = adjust, conf.level = conf.level, na.omit = x$args$na.omit, check = FALSE, output = FALSE)$result

    #—————————————————————————————————————— # #
    ### Create ggplot ####

    # Subtitle
    if (isTRUE(ci)) { if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% ", ifelse(isTRUE(adjust), "Difference-Adjusted ", ""), "Cousineau-Morey Confidence Interval") } else { subtitle } } else { subtitle <- "" }

    p <- ggplot2::ggplot(x$data$long, ggplot2::aes(time, y, group = 1L)) +
      ggplot2::scale_x_discrete(name = xlab) +
      ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = title, subtitle = subtitle) +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5), plot.title = ggplot2::element_text(hjust = 0.5))

    #—————————————————————————————————————— # #
    ### Confidence Intervals ####

    if (isTRUE(ci)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(data = ci.table, ggplot2::aes(variable, m, ymin = low, ymax = upp), width = errorbar.width)) }

    #—————————————————————————————————————— # #
    ### Points ####

    if (isTRUE(point)) { p <- p + suppressWarnings(ggplot2::geom_point(data = ci.table, ggplot2::aes(variable, m), stat = "identity", size = point.size)) }

    #—————————————————————————————————————— # #
    ### Lines ####

    if (isTRUE(line)) { p <- p + suppressWarnings(ggplot2::geom_line(data = ci.table, ggplot2::aes(variable, m), stat = "identity", linewidth = linewidth)) }

    #—————————————————————————————————————— # #
    ### Add jittered points ####

    if (isTRUE(jitter)) { p <- p + suppressWarnings(ggplot2::geom_jitter(alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

    #—————————————————————————————————————— # #
    ### Individual lines ####

    if (isTRUE(jitter && line)) { p <- p + ggplot2::geom_line(data = x$data$long, ggplot2::aes(time, y, group = id), alpha = jitter.alpha, position = ggplot2::position_dodge(jitter.width)) }

  #_____________________________________________________________________________
  #
  # Bollen-Stine Bootstrap with Incomplete Data, boot.bs() ---------------------
  }, "boot.bs" = {

    chisq <- NULL

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
  # (Bootstrap) Confidence Intervals for Correlation Coefficients --------------
  }, "ci.cor" = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    if (isTRUE(is.null(plot))) { plot <- "ci" }
    if (isTRUE(is.null(point.size))) { point.size <- 2.5 }
    if (isTRUE(is.null(point.shape))) { point.shape <- 19 }
    if (isTRUE(is.null(errorbar.width))) { errorbar.width <- 0.3 }
    if (isTRUE(is.null(dodge.width))) { dodge.width <- 0.5 }
    if (isTRUE(is.null(hist.alpha))) { hist.alpha <- 0.4 }
    if (isTRUE(is.null(fill))) { fill <- "gray85" }
    if (isTRUE(is.null(density.col))) { density.col <- "#0072B2" }
    if (isTRUE(is.null(density.linewidth))) { density.linewidth <- 0.5 }
    if (isTRUE(is.null(density.linetype))) { density.linetype <- "solid" }
    if (isTRUE(is.null(point.col))) { point.col <- "#CC79A7" }
    if (isTRUE(is.null(point.linewidth))) { point.linewidth <- 0.6 }
    if (isTRUE(is.null(point.linetype))) { point.linetype <- "solid" }
    if (isTRUE(is.null(ci.col))) { ci.col <- "black" }
    if (isTRUE(is.null(ci.linewidth))) { ci.linewidth <- 0.6 }
    if (isTRUE(is.null(ci.linetype))) { ci.linetype <- "dashed" }
    if (isTRUE(is.null(intercept))) { intercept <- 0L }
    if (isTRUE(is.null(linetype))) { linetype <- "solid" }
    if (isTRUE(is.null(line.col))) { line.col <- "gray65" }
    if (isTRUE(is.null(xbreaks))) { xbreaks <- ggplot2::waiver() }
    if (isTRUE(is.null(ybreaks))) { ybreaks <- ggplot2::waiver() }
    if (isTRUE(is.null(axis.title.size))) { axis.title.size <- 11L }
    if (isTRUE(is.null(axis.text.size))) { axis.text.size <- 10L }
    if (isTRUE(is.null(strip.text.size))) { strip.text.size <- 11L }
    if (isTRUE(is.null(legend.title))) { legend.title <- "" }
    if (isTRUE(is.null(legend.position))) { legend.position <- "bottom" }
    if (isTRUE(is.null(legend.box.margin))) { legend.box.margin <- c(-10L, 0L, 0L, 0L) }
    if (isTRUE(is.null(facet.scales))) { facet.scales <- "free_y" }
    if (is.null(units)) { units <- "in" }

    #—————————————————————————————————————— #
    ### 'xlab', 'ylab', 'xlim' and 'ylim' Argument ####

    switch(plot, "ci" = {

      ylab <- if (isTRUE(is.null(ylab))) { "Correlation Coefficient" } else { ylab }
      ylim <- if (isTRUE(is.null(ylim))) { c(-1, 1) } else { ylim }

    }, "boot" = {

      xlab <- if (isTRUE(is.null(xlab))) { "Correlation Coefficient" } else { xlab }
      ylab <- if (isTRUE(is.null(ylab))) { "Probability Density, f(x)" } else { ylab }
      xlim <- if (isTRUE(is.null(xlim))) { c(-1, 1) } else { xlim }

    })

    #—————————————————————————————————————— #
    ### 'plot.margin' Argument ####

    if (isTRUE(is.null(plot.margin))) { if (isTRUE(is.null(x$data$group))) { plot.margin <- c(5.5, 5.5, 5.5, 5.5) } else { plot.margin <- c(5.5, 5.5, -2.5, 5.5) } }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Correlation Coefficient with Error Bars ####

    switch(plot, "ci" = {

      if (isTRUE(any(!is.na((if (isTRUE(!is.data.frame(x$result))) { do.call("rbind", x$result) } else { x$result })$low)))) {

        p <- .plot.ci(result = x$result, stat = "cor", group = x$data$group, split = x$data$split, point.size = point.size, point.shape = point.shape, errorbar.width = errorbar.width, dodge.width = dodge.width, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        stop("There are no confidence intervals for the correlation coefficient to plot.", call. = FALSE)

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bootstrap Samples ####

    }, "boot" = {

      if (isTRUE(any(!is.na(x$boot$cor)))) {

        p <- .plot.boot(result = x$result, boot.sample = x$boot, stat = "cor", group = x$data$group, split = x$data$split, hist = hist, binwidth = binwidth, bins = bins, alpha = hist.alpha, fill = fill, density = density, density.col = density.col, density.linewidth = density.linewidth, density.linetype = density.linetype, plot.point = point, point.col = point.col, point.linewidth = point.linewidth, point.linetype = point.linetype, plot.ci = ci, ci.col = ci.col, ci.linewidth = ci.linewidth, ci.linetype = ci.linetype, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        stop("There are no bootstrap samples to plot.", call. = FALSE)

      }

    })

  #_____________________________________________________________________________
  #
  # (Bootstrap) Confidence Intervals for Arithmetic Means ----------------------
  }, "ci.mean" = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    if (isTRUE(is.null(plot))) { plot <- "ci" }
    if (isTRUE(is.null(point.size))) { point.size <- 2.5 }
    if (isTRUE(is.null(point.shape))) { point.shape <- 19 }
    if (isTRUE(is.null(errorbar.width))) { errorbar.width <- 0.3 }
    if (isTRUE(is.null(dodge.width))) { dodge.width <- 0.5 }
    if (isTRUE(is.null(hist.alpha))) { hist.alpha <- 0.4 }
    if (isTRUE(is.null(fill))) { fill <- "gray85" }
    if (isTRUE(is.null(density.col))) { density.col <- "#0072B2" }
    if (isTRUE(is.null(density.linewidth))) { density.linewidth <- 0.5 }
    if (isTRUE(is.null(density.linetype))) { density.linetype <- "solid" }
    if (isTRUE(is.null(point.col))) { point.col <- "#CC79A7" }
    if (isTRUE(is.null(point.linewidth))) { point.linewidth <- 0.6 }
    if (isTRUE(is.null(point.linetype))) { point.linetype <- "solid" }
    if (isTRUE(is.null(ci.col))) { ci.col <- "black" }
    if (isTRUE(is.null(ci.linewidth))) { ci.linewidth <- 0.6 }
    if (isTRUE(is.null(ci.linetype))) { ci.linetype <- "dashed" }
    if (isTRUE(is.null(intercept))) { intercept <- 0L }
    if (isTRUE(is.null(linetype))) { linetype <- "solid" }
    if (isTRUE(is.null(line.col))) { line.col <- "gray65" }
    if (isTRUE(is.null(xbreaks))) { xbreaks <- ggplot2::waiver() }
    if (isTRUE(is.null(ybreaks))) { ybreaks <- ggplot2::waiver() }
    if (isTRUE(is.null(axis.title.size))) { axis.title.size <- 11L }
    if (isTRUE(is.null(axis.text.size))) { axis.text.size <- 10L }
    if (isTRUE(is.null(strip.text.size))) { strip.text.size <- 11L }
    if (isTRUE(is.null(legend.title))) { legend.title <- "" }
    if (isTRUE(is.null(legend.position))) { legend.position <- "bottom" }
    if (isTRUE(is.null(legend.box.margin))) { legend.box.margin <- c(-10L, 0L, 0L, 0L) }
    if (isTRUE(is.null(facet.scales))) { facet.scales <- "free_y" }
    if (is.null(units)) { units <- "in" }

    #—————————————————————————————————————— #
    ### 'xlab' and 'ylab' Argument ####

    switch(plot, "ci" = {

      ylab <- if (isTRUE(is.null(ylab))) { "Arithmetic Mean" } else { ylab }

    }, "boot" = {

      xlab <- if (isTRUE(is.null(xlab))) { "Arithmetic Mean" } else { xlab }
      ylab <- if (isTRUE(is.null(ylab))) { "Probability Density, f(x)" } else { ylab }

    })

    #—————————————————————————————————————— #
    ### 'plot.margin' Argument ####

    if (isTRUE(is.null(plot.margin))) { if (isTRUE(is.null(x$data$group))) { plot.margin <- c(5.5, 5.5, 5.5, 5.5) } else { plot.margin <- c(5.5, 5.5, -2.5, 5.5) } }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Plot Arithmetic Mean with Error Bars ####

    switch(plot, "ci" = {

      if (isTRUE(any(!is.na((if (isTRUE(!is.data.frame(x$result))) { do.call("rbind", x$result) } else { x$result })$low)))) {

        p <- .plot.ci(result = x$result, stat = "m", group = x$data$group, split = x$data$split, point.size = point.size, point.shape = point.shape, errorbar.width = errorbar.width, dodge.width = dodge.width, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        plot <- "none"

        warning("There are no confidence intervals for the arithmetic mean to plot.", call. = FALSE)

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Plot Bootstrap Samples ####

    }, "boot" = {

      if (any(!is.na(x$boot$m))) {

        p <- .plot.boot(result = x$result, boot.sample = x$boot, stat = "m", group = x$data$group, split = x$data$split, hist = hist, binwidth = binwidth, bins = bins, alpha = hist.alpha, fill = fill, density = density, density.col = density.col, density.linewidth = density.linewidth, density.linetype = density.linetype, plot.point = point, point.col = point.col, point.linewidth = point.linewidth, point.linetype = point.linetype, plot.ci = ci, ci.col = ci.col, ci.linewidth = ci.linewidth, ci.linetype = ci.linetype, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p


      } else {

        plot <- "none"

        warning("There are no bootstrap samples to plot.", call. = FALSE)

      }

    })

  #_____________________________________________________________________________
  #
  # (Bootstrap) Confidence Intervals for Median --------------------------------
  }, "ci.median" = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    if (isTRUE(is.null(plot))) { plot <- "ci" }
    if (isTRUE(is.null(point.size))) { point.size <- 2.5 }
    if (isTRUE(is.null(point.shape))) { point.shape <- 19 }
    if (isTRUE(is.null(errorbar.width))) { errorbar.width <- 0.3 }
    if (isTRUE(is.null(dodge.width))) { dodge.width <- 0.5 }
    if (isTRUE(is.null(hist.alpha))) { hist.alpha <- 0.4 }
    if (isTRUE(is.null(fill))) { fill <- "gray85" }
    if (isTRUE(is.null(density.col))) { density.col <- "#0072B2" }
    if (isTRUE(is.null(density.linewidth))) { density.linewidth <- 0.5 }
    if (isTRUE(is.null(density.linetype))) { density.linetype <- "solid" }
    if (isTRUE(is.null(point.col))) { point.col <- "#CC79A7" }
    if (isTRUE(is.null(point.linewidth))) { point.linewidth <- 0.6 }
    if (isTRUE(is.null(point.linetype))) { point.linetype <- "solid" }
    if (isTRUE(is.null(ci.col))) { ci.col <- "black" }
    if (isTRUE(is.null(ci.linewidth))) { ci.linewidth <- 0.6 }
    if (isTRUE(is.null(ci.linetype))) { ci.linetype <- "dashed" }
    if (isTRUE(is.null(intercept))) { intercept <- 0L }
    if (isTRUE(is.null(linetype))) { linetype <- "solid" }
    if (isTRUE(is.null(line.col))) { line.col <- "gray65" }
    if (isTRUE(is.null(xbreaks))) { xbreaks <- ggplot2::waiver() }
    if (isTRUE(is.null(ybreaks))) { ybreaks <- ggplot2::waiver() }
    if (isTRUE(is.null(axis.title.size))) { axis.title.size <- 11L }
    if (isTRUE(is.null(axis.text.size))) { axis.text.size <- 10L }
    if (isTRUE(is.null(strip.text.size))) { strip.text.size <- 11L }
    if (isTRUE(is.null(legend.title))) { legend.title <- "" }
    if (isTRUE(is.null(legend.position))) { legend.position <- "bottom" }
    if (isTRUE(is.null(legend.box.margin))) { legend.box.margin <- c(-10L, 0L, 0L, 0L) }
    if (isTRUE(is.null(facet.scales))) { facet.scales <- "free_y" }
    if (is.null(units)) { units <- "in" }

    #—————————————————————————————————————— #
    ### 'xlab' and 'ylab' Argument ####

    switch(plot, "ci" = {

      ylab <- if (isTRUE(is.null(ylab))) { "Arithmetic Mean" } else { ylab }

    }, "boot" = {

      xlab <- if (isTRUE(is.null(xlab))) { "Arithmetic Mean" } else { xlab }
      ylab <- if (isTRUE(is.null(ylab))) { "Probability Density, f(x)" } else { ylab }

    })

    #—————————————————————————————————————— #
    ### 'plot.margin' Argument ####

    if (isTRUE(is.null(plot.margin))) { if (isTRUE(is.null(x$data$group))) { plot.margin <- c(5.5, 5.5, 5.5, 5.5) } else { plot.margin <- c(5.5, 5.5, -2.5, 5.5) } }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Plot Median with Error Bars ####

    switch(plot, "ci" = {

      if (isTRUE(any(!is.na((if (isTRUE(!is.data.frame(x$result))) { do.call("rbind", x$result) } else { x$result })$low)))) {

        p <- .plot.ci(result = x$result, stat = "med", group = x$data$group, split = x$data$split, point.size = point.size, point.shape = point.shape, errorbar.width = errorbar.width, dodge.width = dodge.width, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        warning("There are no confidence intervals for the median to plot.", call. = FALSE)

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Plot Bootstrap Samples ####

    }, "boot" = {

      if (any(!is.na(x$boot$med))) {

        p <- .plot.boot(result = x$result, boot.sample = x$boot, stat = "med", group = x$data$group, split = x$data$split, hist = hist, binwidth = binwidth, bins = bins, alpha = hist.alpha, fill = fill, density = density, density.col = density.col, density.linewidth = density.linewidth, density.linetype = density.linetype, plot.point = point, point.col = point.col, point.linewidth = point.linewidth, point.linetype = point.linetype, plot.ci = ci, ci.col = ci.col, ci.linewidth = ci.linewidth, ci.linetype = ci.linetype, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        warning("There are no bootstrap samples to plot.", call. = FALSE)

      }

    })

  #_____________________________________________________________________________
  #
  # (Bootstrap) Confidence Intervals for Proportions ---------------------------
  }, "ci.prop" = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    if (isTRUE(is.null(plot))) { plot <- "ci" }
    if (isTRUE(is.null(point.size))) { point.size <- 2.5 }
    if (isTRUE(is.null(point.shape))) { point.shape <- 19 }
    if (isTRUE(is.null(errorbar.width))) { errorbar.width <- 0.3 }
    if (isTRUE(is.null(dodge.width))) { dodge.width <- 0.5 }
    if (isTRUE(is.null(hist.alpha))) { hist.alpha <- 0.4 }
    if (isTRUE(is.null(fill))) { fill <- "gray85" }
    if (isTRUE(is.null(density.col))) { density.col <- "#0072B2" }
    if (isTRUE(is.null(density.linewidth))) { density.linewidth <- 0.5 }
    if (isTRUE(is.null(density.linetype))) { density.linetype <- "solid" }
    if (isTRUE(is.null(point.col))) { point.col <- "#CC79A7" }
    if (isTRUE(is.null(point.linewidth))) { point.linewidth <- 0.6 }
    if (isTRUE(is.null(point.linetype))) { point.linetype <- "solid" }
    if (isTRUE(is.null(ci.col))) { ci.col <- "black" }
    if (isTRUE(is.null(ci.linewidth))) { ci.linewidth <- 0.6 }
    if (isTRUE(is.null(ci.linetype))) { ci.linetype <- "dashed" }
    if (isTRUE(is.null(intercept))) { intercept <- 0L }
    if (isTRUE(is.null(linetype))) { linetype <- "solid" }
    if (isTRUE(is.null(line.col))) { line.col <- "gray65" }
    if (isTRUE(is.null(xbreaks))) { xbreaks <- ggplot2::waiver() }
    if (isTRUE(is.null(ybreaks))) { ybreaks <- ggplot2::waiver() }
    if (isTRUE(is.null(axis.title.size))) { axis.title.size <- 11L }
    if (isTRUE(is.null(axis.text.size))) { axis.text.size <- 10L }
    if (isTRUE(is.null(strip.text.size))) { strip.text.size <- 11L }
    if (isTRUE(is.null(legend.title))) { legend.title <- "" }
    if (isTRUE(is.null(legend.position))) { legend.position <- "bottom" }
    if (isTRUE(is.null(legend.box.margin))) { legend.box.margin <- c(-10L, 0L, 0L, 0L) }
    if (isTRUE(is.null(facet.scales))) { facet.scales <- "free_y" }
    if (is.null(units)) { units <- "in" }

    #—————————————————————————————————————— #
    ### 'xlab', 'ylab', and 'ylim' Argument ####

    switch(plot, "ci" = {

      ylab <- if (isTRUE(is.null(ylab))) { "Proportion" } else { ylab }
      ylim <- if (isTRUE(is.null(ylim))) { c(0, 1) } else { ylim }

    }, "boot" = {

      xlab <- if (isTRUE(is.null(xlab))) { "Proportion" } else { xlab }
      ylab <- if (isTRUE(is.null(ylab))) { "Probability Density, f(x)" } else { ylab }
      xlim <- if (isTRUE(is.null(xlim))) { c(0, 1) } else { xlim }

    })

    #—————————————————————————————————————— #
    ### 'plot.margin' Argument ####

    if (isTRUE(is.null(plot.margin))) { if (isTRUE(is.null(x$data$group))) { plot.margin <- c(5.5, 5.5, 5.5, 5.5) } else { plot.margin <- c(5.5, 5.5, -2.5, 5.5) } }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Proportion with Error Bars ####

    switch(plot, "ci" = {

      if (isTRUE(any(!is.na((if (isTRUE(!is.data.frame(x$result))) { do.call("rbind", x$result) } else { x$result })$low)))) {

        p <- .plot.ci(result = x$result, stat = "prop", group = x$data$group, split = x$data$split, point.size = point.size, point.shape = point.shape, errorbar.width = errorbar.width, dodge.width = dodge.width, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        warning("There are no confidence intervals for the proportion to plot.", call. = FALSE)

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bootstrap Samples ####

    }, "boot" = {

      if (any(!is.na(x$boot$prop))) {

        p <- .plot.boot(result = x$result, boot.sample = x$boot, stat = "prop", group = x$data$group, split = x$data$split, hist = hist, binwidth = binwidth, bins = bins, alpha = hist.alpha, fill = fill, density = density, density.col = density.col, density.linewidth = density.linewidth, density.linetype = density.linetype, plot.point = point, point.col = point.col, point.linewidth = point.linewidth, point.linetype = point.linetype, plot.ci = ci, ci.col = ci.col, ci.linewidth = ci.linewidth, ci.linetype = ci.linetype, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        warning("There are no bootstrap samples to plot.", call. = FALSE)

      }

    })

  #_____________________________________________________________________________
  #
  # (Bootstrap) Confidence Intervals for Variances -----------------------------
  }, "ci.var" = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    if (isTRUE(is.null(plot))) { plot <- "ci" }
    if (isTRUE(is.null(point.size))) { point.size <- 2.5 }
    if (isTRUE(is.null(point.shape))) { point.shape <- 19 }
    if (isTRUE(is.null(errorbar.width))) { errorbar.width <- 0.3 }
    if (isTRUE(is.null(dodge.width))) { dodge.width <- 0.5 }
    if (isTRUE(is.null(hist.alpha))) { hist.alpha <- 0.4 }
    if (isTRUE(is.null(fill))) { fill <- "gray85" }
    if (isTRUE(is.null(density.col))) { density.col <- "#0072B2" }
    if (isTRUE(is.null(density.linewidth))) { density.linewidth <- 0.5 }
    if (isTRUE(is.null(density.linetype))) { density.linetype <- "solid" }
    if (isTRUE(is.null(point.col))) { point.col <- "#CC79A7" }
    if (isTRUE(is.null(point.linewidth))) { point.linewidth <- 0.6 }
    if (isTRUE(is.null(point.linetype))) { point.linetype <- "solid" }
    if (isTRUE(is.null(ci.col))) { ci.col <- "black" }
    if (isTRUE(is.null(ci.linewidth))) { ci.linewidth <- 0.6 }
    if (isTRUE(is.null(ci.linetype))) { ci.linetype <- "dashed" }
    if (isTRUE(is.null(intercept))) { intercept <- 0L }
    if (isTRUE(is.null(linetype))) { linetype <- "solid" }
    if (isTRUE(is.null(line.col))) { line.col <- "gray65" }
    if (isTRUE(is.null(xbreaks))) { xbreaks <- ggplot2::waiver() }
    if (isTRUE(is.null(ybreaks))) { ybreaks <- ggplot2::waiver() }
    if (isTRUE(is.null(axis.title.size))) { axis.title.size <- 11L }
    if (isTRUE(is.null(axis.text.size))) { axis.text.size <- 10L }
    if (isTRUE(is.null(strip.text.size))) { strip.text.size <- 11L }
    if (isTRUE(is.null(legend.title))) { legend.title <- "" }
    if (isTRUE(is.null(legend.position))) { legend.position <- "bottom" }
    if (isTRUE(is.null(legend.box.margin))) { legend.box.margin <- c(-10L, 0L, 0L, 0L) }
    if (isTRUE(is.null(facet.scales))) { facet.scales <- "free_y" }
    if (is.null(units)) { units <- "in" }

    #—————————————————————————————————————— #
    ### 'xlab' and 'ylab' Argument ####

    switch(plot, "ci" = {

      ylab <- if (isTRUE(is.null(ylab))) { "Variance" } else { ylab }

    }, "boot" = {

      xlab <- if (isTRUE(is.null(xlab))) { "Variance" } else { xlab }
      ylab <- if (isTRUE(is.null(ylab))) { "Probability Density, f(x)" } else { ylab }

    })

    #—————————————————————————————————————— #
    ### 'plot.margin' Argument ####

    if (isTRUE(is.null(plot.margin))) { if (isTRUE(is.null(x$data$group))) { plot.margin <- c(5.5, 5.5, 5.5, 5.5) } else { plot.margin <- c(5.5, 5.5, -2.5, 5.5) } }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Variance with Error Bars ####

    switch(plot, "ci" = {

      if (isTRUE(any(!is.na((if (isTRUE(!is.data.frame(x$result))) { do.call("rbind", x$result) } else { x$result })$low)))) {

        p <- .plot.ci(result = x$result, stat = "var", group = x$data$group, split = x$data$split, point.size = point.size, point.shape = point.shape, errorbar.width = errorbar.width, dodge.width = dodge.width, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        warning("There are no confidence intervals for the variance to plot.", call. = FALSE)

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bootstrap Samples ####

    }, "boot" = {

      if (any(!is.na(x$boot$var))) {

        p <- .plot.boot(result = x$result, boot.sample = x$boot, stat = "var", group = x$data$group, split = x$data$split, hist = hist, binwidth = binwidth, bins = bins, alpha = hist.alpha, fill = fill, density = density, density.col = density.col, density.linewidth = density.linewidth, density.linetype = density.linetype, plot.point = point, point.col = point.col, point.linewidth = point.linewidth, point.linetype = point.linetype, plot.ci = ci, ci.col = ci.col, ci.linewidth = ci.linewidth, ci.linetype = ci.linetype, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        warning("There are no bootstrap samples to plot.", call. = FALSE)

      }

    })

  #_____________________________________________________________________________
  #
  # (Bootstrap) Confidence Intervals for Standard Deviations -------------------
  }, "ci.sd" = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    if (isTRUE(is.null(plot))) { plot <- "ci" }
    if (isTRUE(is.null(point.size))) { point.size <- 2.5 }
    if (isTRUE(is.null(point.shape))) { point.shape <- 19 }
    if (isTRUE(is.null(errorbar.width))) { errorbar.width <- 0.3 }
    if (isTRUE(is.null(dodge.width))) { dodge.width <- 0.5 }
    if (isTRUE(is.null(hist.alpha))) { hist.alpha <- 0.4 }
    if (isTRUE(is.null(fill))) { fill <- "gray85" }
    if (isTRUE(is.null(density.col))) { density.col <- "#0072B2" }
    if (isTRUE(is.null(density.linewidth))) { density.linewidth <- 0.5 }
    if (isTRUE(is.null(density.linetype))) { density.linetype <- "solid" }
    if (isTRUE(is.null(point.col))) { point.col <- "#CC79A7" }
    if (isTRUE(is.null(point.linewidth))) { point.linewidth <- 0.6 }
    if (isTRUE(is.null(point.linetype))) { point.linetype <- "solid" }
    if (isTRUE(is.null(ci.col))) { ci.col <- "black" }
    if (isTRUE(is.null(ci.linewidth))) { ci.linewidth <- 0.6 }
    if (isTRUE(is.null(ci.linetype))) { ci.linetype <- "dashed" }
    if (isTRUE(is.null(intercept))) { intercept <- 0L }
    if (isTRUE(is.null(linetype))) { linetype <- "solid" }
    if (isTRUE(is.null(line.col))) { line.col <- "gray65" }
    if (isTRUE(is.null(xbreaks))) { xbreaks <- ggplot2::waiver() }
    if (isTRUE(is.null(ybreaks))) { ybreaks <- ggplot2::waiver() }
    if (isTRUE(is.null(axis.title.size))) { axis.title.size <- 11L }
    if (isTRUE(is.null(axis.text.size))) { axis.text.size <- 10L }
    if (isTRUE(is.null(strip.text.size))) { strip.text.size <- 11L }
    if (isTRUE(is.null(legend.title))) { legend.title <- "" }
    if (isTRUE(is.null(legend.position))) { legend.position <- "bottom" }
    if (isTRUE(is.null(legend.box.margin))) { legend.box.margin <- c(-10L, 0L, 0L, 0L) }
    if (isTRUE(is.null(facet.scales))) { facet.scales <- "free_y" }
    if (is.null(units)) { units <- "in" }

    #—————————————————————————————————————— #
    ### 'xlab' and 'ylab' Argument ####

    switch(plot, "ci" = {

      ylab <- if (isTRUE(is.null(ylab))) { "Variance" } else { ylab }

    }, "boot" = {

      xlab <- if (isTRUE(is.null(xlab))) { "Variance" } else { xlab }
      ylab <- if (isTRUE(is.null(ylab))) { "Probability Density, f(x)" } else { ylab }

    })

    #—————————————————————————————————————— #
    ### 'plot.margin' Argument ####

    if (isTRUE(is.null(plot.margin))) { if (isTRUE(is.null(x$data$group))) { plot.margin <- c(5.5, 5.5, 5.5, 5.5) } else { plot.margin <- c(5.5, 5.5, -2.5, 5.5) } }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Standard Deviation with Error Bars ####

    switch(plot, "ci" = {

      if (isTRUE(any(!is.na((if (isTRUE(!is.data.frame(x$result))) { do.call("rbind", x$result) } else { x$result })$low)))) {

        p <- .plot.ci(result = x$result, stat = "sd", group = x$data$group, split = x$data$split, point.size = point.size, point.shape = point.shape, errorbar.width = errorbar.width, dodge.width = dodge.width, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        warning("There are no confidence intervals for the standard deviation to plot.", call. = FALSE)

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bootstrap Samples ####

    }, "boot" = {

      if (any(!is.na(x$boot$sd))) {

        p <- .plot.boot(result = x$result, boot.sample = x$boot, stat = "sd", group = x$datagroup, split = x$data$split, hist = hist, binwidth = binwidth, bins = bins, alpha = hist.alpha, fill = fill, density = density, density.col = density.col, density.linewidth = density.linewidth, density.linetype = density.linetype, plot.point = point, point.col = point.col, point.linewidth = point.linewidth, point.linetype = point.linetype, plot.ci = ci, ci.col = ci.col, ci.linewidth = ci.linewidth, ci.linetype = ci.linetype, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        warning("There are no bootstrap samples to plot.", call. = FALSE)

      }

    })

  #_____________________________________________________________________________
  #
  # Dynamic Fit Index Cutoffs --------------------------------------------------
  }, "item.dfi" = {

    fit <- cutoff <- NULL

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    if (is.null(units)) { units <- "in" }

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
    ## Plot ####

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
  # R-Squared Measures for Multilevel and LMMs ---------------------------------
  }, "multilevel.r2" = {

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

      p <- p + ggplot2::scale_fill_manual(values = rev(color))

    }

  #_____________________________________________________________________________
  #
  # R-Squared Measures for Multilevel and LMMS, Manual Input -------------------
  }, "multilevel.r2.manual" = {

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

      p <- p + ggplot2::scale_fill_manual(values = rev(color))

    }

  #_____________________________________________________________________________
  #
  # Missing Data Pattern  ------------------------------------------------------
  }, "na.pattern" = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Plot Data ####

    plotdat <- do.call("rbind", apply(x$result[-nrow(x$result), c("pattern", colnames(x$data))], 1, function(y) {

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
      ggplot2::scale_fill_manual(values = c("Missing" = color[1L], "Observed" = color[2L])) +
      ggplot2::scale_alpha_continuous(limits = c(0, 1), guide = "none") +
      ggplot2::scale_x_continuous("Number of Missing Entries per Variable",
                                  breaks = seq_len(length(colnames(x$data))),
                                  labels = as.character(x$result[nrow(x$result), colnames(x$data)]),
                                  sec.axis = ggplot2::dup_axis(labels = colnames(x$data), name = "Variable")) +
      ggplot2::scale_y_reverse("Pattern Frequency",
                                breaks = seq_len(length(x$result$n) - 1L), labels = x$result[-nrow(x$result), "n"],
                                sec.axis = ggplot2::dup_axis(labels = x$result[-nrow(x$result), "nNA"], name = "Number of Missing Entries per Pattern")) +
      ggplot2::theme(plot.margin = ggplot2::margin(t = plot.margin[1L], r = plot.margin[2L], b = plot.margin[3L], l = plot.margin[4L]),
                     legend.title = ggplot2::element_blank(),
                     legend.position = "bottom",
                     legend.key.size = ggplot2::unit(legend.key.size, "pt"),
                     legend.text = ggplot2::element_text(size = legend.text.size),
                     legend.box.margin = ggplot2::margin(t = legend.box.margin[1L], r = legend.box.margin[2L], b = legend.box.margin[3L], l = legend.box.margin[4L]),
                     panel.grid.minor = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank())

    ### Square Plot Tiles ####
    if (isTRUE(square)) { p <- p + ggplot2::coord_fixed(expand = FALSE) } else { p <- p + ggplot2::coord_cartesian(expand = FALSE) }

    ### Rotate Labels ####
    if (isTRUE(rotate)) { p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90L)) }

    ### Caption ####
    if (isTRUE(length(n.pattern.exclude) != 0L)) {

      p <- p + ggplot2::labs(caption = paste0("Note. ", length(n.pattern.exclude), ifelse(length(n.pattern.exclude) == 1L, " pattern ", " patterns "), " with less than ", n.pattern, " cases removed.")) +
        ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0.5, vjust = 5))

    }

  #_____________________________________________________________________________
  #
  # Levene's Test for Homogeneity of Variance ----------------------------------
  }, "test.levene" = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    if (is.null(violin.alpha)) { violin.alpha <- 0.3 }
    if (is.null(violin.trim)) { violin.trim <- FALSE }
    if (is.null(box.alpha)) { box.alpha <- 0.2 }
    if (is.null(box.width)) { box.width <- 0.2 }
    if (is.null(jitter.size)) { jitter.size <- 1.25 }
    if (is.null(jitter.width)) { jitter.width <- 0.05 }
    if (is.null(jitter.height)) { jitter.height <- 0 }
    if (is.null(jitter.alpha)) { jitter.alpha <- 0.2 }
    if (is.null(gray)) { gray <- FALSE }
    if (is.null(start)) { start <- 0.9 }
    if (is.null(end)) { end <- 0.4 }
    if (is.null(ylab)) { ylab <- "y" }
    if (is.null(ybreaks)) { ybreaks <- ggplot2::waiver() }
    if (is.null(units)) { units <- "in" }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Create Plot ####

    p <- ggplot2::ggplot(x$data, ggplot2::aes(group, y, fill = group)) +
      ggplot2::scale_x_discrete(name = xlab) +
      ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = title, subtitle = subtitle) +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5),
                     plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::guides(fill = "none")

    # Add violin plots
    if (isTRUE(violin)) { p <- p + ggplot2::geom_violin(alpha = violin.alpha, trim = violin.trim) }

    # Add boxplots
    if (isTRUE(box)) { p <- p + ggplot2::geom_boxplot(alpha = box.alpha, width = box.width) }

    # Add jittered points
    if (isTRUE(jitter)) { p <- p + ggplot2::geom_jitter(alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size) }

    # Gray color scales
    if (isTRUE(gray)) {

      p <- p + ggplot2::scale_fill_grey(start = start, end = end)

    # User-specified colors
    } else {

      if (isTRUE(!is.null(color))) { p <- p + ggplot2::scale_fill_manual(values = color) }

    }

  #_____________________________________________________________________________
  #
  # t-Test ---------------------------------------------------------------------
  }, "test.t" = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    if (is.null(point.size)) { point.size <- 3 }
    if (is.null(errorbar.width)) { errorbar.width <- 0.1 }
    if (is.null(linetype)) { linetype <- 3 }
    if (is.null(linewidth)) { linewidth <- 0.8 }
    if (is.null(jitter.size)) { jitter.size <- 1.25 }
    if (is.null(jitter.width)) { jitter.width <- 0.05 }
    if (is.null(jitter.height)) { jitter.height <- 0 }
    if (is.null(jitter.alpha)) { jitter.alpha <- 0.1 }
    if (is.null(subtitle)) { subtitle <- "Confidence Interval" }
    if (is.null(ylab)) { ylab <- "y" }
    if (is.null(ybreaks)) { ybreaks <- ggplot2::waiver() }
    if (is.null(units)) { units <- "in" }

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

             if (isTRUE(bar)) { p <- p + suppressWarnings(ggplot2::geom_bar(data = x$result, ggplot2::aes(0, m), stat = "summary", fun = "mean")) }

             #...................
             #### Point ####

             if (isTRUE(point)) { p <- p + suppressWarnings(ggplot2::geom_point(data = x$result, ggplot2::aes(0, m), stat = "identity", size = point.size)) }

             #...................
             #### Confidence Intervals ####

             if (isTRUE(ci)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(data = x$result, ggplot2::aes(0, m, ymin = m.low, ymax = m.upp), width = errorbar.width)) }

             #...................
             #### Jittered Points ####

             if (isTRUE(jitter)) { p <- p + suppressWarnings(ggplot2::geom_jitter(data = data.frame(x = x$data$x), ggplot2::aes(x = 0L, y = x), alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

             #...................
             #### Horizontal Line ####

             if (isTRUE(line)) { p <- p + ggplot2::geom_hline(data = data.frame(mu = x$args$mu), ggplot2::aes(yintercept = mu), linetype = linetype, linewidth = linewidth) }

           #—————————————————————————————————————— #
           ### Two-Sample ####

           }, "two" = {

             # Plot data
             plotdat <- split(x$data[, 1L], f = x$data[, 2L]) |> (\(y) data.frame(group = factor(c(rep(x$result[1L, "group"], times = length(y[[1L]])), rep(x$result[2L, "group"], times = length(y[[2L]])))), y = unlist(y)) )()

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

             if (isTRUE(bar)) { p <- p + suppressWarnings(ggplot2::geom_bar(ggplot2::aes(group, m), stat = "summary", fun = "mean")) }

             #...................
             #### Point ####

             if (isTRUE(point)) { p <- p + suppressWarnings(ggplot2::geom_point(ggplot2::aes(group, m), stat = "identity", size = point.size)) }

             #...................
             #### Confidence Intervals ####

             if (isTRUE(ci)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(ggplot2::aes(group, m, ymin = low, ymax = upp), width = errorbar.width)) }

             #...................
             #### Jittered Points ####

             if (isTRUE(jitter)) { p <- p + suppressWarnings(ggplot2::geom_jitter(data = plotdat, ggplot2::aes(x = group, y = y), alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

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

             if (isTRUE(bar)) { p <- p + suppressWarnings(ggplot2::geom_bar(data = x$result, ggplot2::aes(0, m.diff), stat = "summary", fun = "mean")) }

             #...................
             #### Point ####

             if (isTRUE(point)) { p <- p + suppressWarnings(ggplot2::geom_point(data = x$result, ggplot2::aes(0, m.diff), stat = "identity", size = point.size)) }

             #...................
             #### Confidence Intervals ####

             if (isTRUE(ci)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(data = x$result, ggplot2::aes(0, m.diff, ymin = m.low, ymax = m.upp), width = errorbar.width)) }

             #...................
             #### Jittered Points ####

             if (isTRUE(jitter)) { p <- p + suppressWarnings(ggplot2::geom_jitter(data = data.frame(x = x$data$y - x$data$x), ggplot2::aes(x = 0L, y = x), alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

             #...................
             #### Horizontal Line ####

             if (isTRUE(line)) { p <- p + ggplot2::geom_hline(yintercept = 0L, linetype = linetype, linewidth = linewidth) }

           })

  #_____________________________________________________________________________
  #
  # Welch's Test ---------------------------------------------------------------
  }, test.welch = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    if (is.null(point.size)) { point.size <- 3 }
    if (is.null(errorbar.width)) { errorbar.width <- 0.1 }
    if (is.null(jitter.size)) { jitter.size <- 1.25 }
    if (is.null(jitter.width)) { jitter.width <- 0.05 }
    if (is.null(jitter.height)) { jitter.height <- 0 }
    if (is.null(jitter.alpha)) { jitter.alpha <- 0.1 }
    if (is.null(jitter.height)) { jitter.height <- 0 }
    if (is.null(jitter.alpha)) { jitter.alpha <- 0.2 }
    if (is.null(subtitle)) { subtitle <- "Confidence Interval" }
    if (is.null(ylab)) { ylab <- "y" }
    if (is.null(ybreaks)) { ybreaks <- ggplot2::waiver() }
    if (is.null(units)) { units <- "in" }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Means and (Difference-Adjusted) Confidence Intervals by Group ####

    ci.table <- x$result$descript

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Create ggplot ####

    # Subtitle
    if (isTRUE(ci)) { if (isTRUE(subtitle == "Confidence Intervals")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% ", ifelse(isTRUE(adjust), "Difference-Adjusted ", ""), "Confidence Intervals") } else { subtitle } } else { subtitle <- "" }

    p <- ggplot2::ggplot(x$data, ggplot2::aes(group, y)) +
      ggplot2::scale_x_discrete(name = xlab) +
      ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = title, subtitle = subtitle) +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5), plot.title = ggplot2::element_text(hjust = 0.5))

    #—————————————————————————————————————— #
    ### Bars ####

    if (isTRUE(bar)) { p <- p + suppressWarnings(ggplot2::geom_bar(data = ci.table, ggplot2::aes(group, m), stat = "summary", fun = "mean")) }

    #—————————————————————————————————————— #
    ### Confidence Intervals ####

    if (isTRUE(ci)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(data = ci.table, ggplot2::aes(group, m, ymin = low, ymax = upp), width = errorbar.width)) }

    #—————————————————————————————————————— #
    ### Points ####

    if (isTRUE(point)) { p <- p + suppressWarnings(ggplot2::geom_point(data = ci.table, ggplot2::aes(group, m), stat = "identity", size = point.size)) }

    #—————————————————————————————————————— #
    ### Jittered Points ####

    if (isTRUE(jitter)) { p <- p + suppressWarnings(ggplot2::geom_jitter(data = x$data, ggplot2::aes(group, y), alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

  #_____________________________________________________________________________
  #
  # z-Test ---------------------------------------------------------------------
  }, "test.z" = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Default Argument Specification ####

    if (is.null(point.size)) { point.size <- 3 }
    if (is.null(errorbar.width)) { errorbar.width <- 0.1 }
    if (is.null(linetype)) { linetype <- 3 }
    if (is.null(linewidth)) { linewidth <- 0.8 }
    if (is.null(jitter.size)) { jitter.size <- 1.25 }
    if (is.null(jitter.width)) { jitter.width <- 0.05 }
    if (is.null(jitter.height)) { jitter.height <- 0 }
    if (is.null(jitter.alpha)) { jitter.alpha <- 0.1 }
    if (is.null(subtitle)) { subtitle <- "Confidence Interval" }
    if (is.null(ylab)) { ylab <- "y" }
    if (is.null(ybreaks)) { ybreaks <- ggplot2::waiver() }
    if (is.null(units)) { units <- "in" }

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

             if (isTRUE(bar)) { p <- p + suppressWarnings(ggplot2::geom_bar(data = x$result, ggplot2::aes(0L, m), stat = "summary", fun = "mean")) }

             #···················
             #### Point ####

             if (isTRUE(point)) { p <- p + suppressWarnings(ggplot2::geom_point(data = x$result, ggplot2::aes(0L, m), stat = "identity", size = point.size)) }

             #...................
             #### Confidence Intervals ####

             if (isTRUE(ci)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(data = x$result, ggplot2::aes(0L, m, ymin = m.low, ymax = m.upp), width = errorbar.width)) }

             #...................
             #### Jittered Points ####

             if (isTRUE(jitter)) { p <- p + suppressWarnings(ggplot2::geom_jitter(data = data.frame(x = x$data$x), ggplot2::aes(x = 0L, y = x), alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

             #...................
             #### Horizontal Line ####

             if (isTRUE(line)) { p <- p + ggplot2::geom_hline(data = data.frame(mu = x$args$mu), ggplot2::aes(yintercept = mu), linetype = linetype, linewidth = linewidth) }

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

             if (isTRUE(bar)) { p <- p + suppressWarnings(ggplot2::geom_bar(ggplot2::aes(group, m), stat = "summary", fun = "mean")) }

             #...................
             #### Point ####

             if (isTRUE(point)) { p <- p + suppressWarnings(ggplot2::geom_point(ggplot2::aes(group, m), stat = "identity", size = point.size)) }

             #...................
             #### Confidence Intervals ####

             if (isTRUE(ci)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(ggplot2::aes(group, m, ymin = low, ymax = upp), width = errorbar.width)) }

             #...................
             #### Jittered Points ####

             if (isTRUE(jitter)) { p <- p + suppressWarnings(ggplot2::geom_jitter(data = plotdat, ggplot2::aes(x = group, y = y), alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

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

             if (isTRUE(bar)) { p <- p + suppressWarnings(ggplot2::geom_bar(data = x$result, ggplot2::aes(0L, m.diff), stat = "summary", fun = "mean")) }

             #...................
             #### Point ####

             if (isTRUE(point)) { p <- p + suppressWarnings(ggplot2::geom_point(data = x$result, ggplot2::aes(0L, m.diff), stat = "identity", size = point.size)) }

             #...................
             #### Confidence Intervals ####

             if (isTRUE(ci)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(data = x$result, ggplot2::aes(0L, m.diff, ymin = m.low, ymax = m.upp), width = errorbar.width)) }

             #...................
             #### Jittered Points ####

             if (isTRUE(jitter)) { p <- p + suppressWarnings(ggplot2::geom_jitter(data = data.frame(x = x$data$y - x$data$x), ggplot2::aes(x = 0L, y = x), alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

             #...................
             #### Horizontal Line ####

             if (isTRUE(line)) { p <- p + ggplot2::geom_hline(yintercept = 0L, linetype = linetype, linewidth = linewidth) }

           })

  })

  #_____________________________________________________________________________
  #
  # Save Plot ------------------------------------------------------------------

  if (isTRUE(!is.null(filename))) { suppressWarnings(suppressMessages(ggplot2::ggsave(filename = filename, plot = p, width = width, height = height, units = units, dpi = dpi))) }

  #_____________________________________________________________________________
  #
  # Plot -----------------------------------------------------------------------

  return(p)

}

#_______________________________________________________________________________
