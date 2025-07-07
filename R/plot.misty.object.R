#' Plots misty.object object
#'
#' This function plots an \code{misty.object} object.
#'
#' @param x                  \code{misty.object} object.
#' @param plot               see 'Arguments' in the functions \code{ci.*} (e.g.,
#'                           \code{\link{ci.cor}}).
#' @param bar                see 'Arguments' in the functions \code{\link{aov.b}},
#'                           \code{\link{test.t}}, or \code{\link{test.welch}}.
#' @param box                see 'Arguments' in the function \code{\link{test.levene}}.
#' @param violin             see 'Arguments' in the function \code{\link{test.levene}}.
#' @param hist               see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param point              see 'Arguments' in the functions \code{\link{aov.b}},
#'                           \code{ci.*} (e.g., \code{\link{ci.cor}}), or \code{\link{test.welch}}.
#' @param line               see 'Arguments' in the functions \code{\link{aov.w}},
#'                           \code{ci.*} (e.g., \code{\link{ci.cor}}), or \code{\link{test.t}}.
#' @param ci                 see 'Arguments' in the functions \code{\link{aov.b}},
#'                           \code{\link{aov.w}}, \code{ci.*} (e.g., \code{\link{ci.cor}}),
#'                           \code{\link{test.t}}, or \code{\link{test.welch}}.
#' @param conf.level         see 'Arguments' in the functions \code{\link{aov.b}},
#'                           \code{\link{aov.w}}, \code{ci.*} (e.g., \code{\link{ci.cor}}),
#'                           \code{\link{test.t}}, or \code{\link{test.welch}}.
#' @param adjust             see 'Arguments' in the functions \code{\link{aov.b}},
#'                           \code{\link{aov.w}}, \code{\link{test.t}}, or
#'                           \code{\link{test.welch}},
#' @param jitter             see 'Arguments' in the functions \code{\link{aov.b}},
#'                           \code{\link{aov.w}},  \code{\link{test.t}},
#'                           \code{\link{test.levene}},or \code{\link{test.welch}}.
#' @param density            see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param square             see 'Arguments' in the function \code{\link{na.pattern}}.
#' @param rotate             see 'Arguments' in the function \code{\link{na.pattern}}.
#' @param binwidth           see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param bins               see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param fill               see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param hist.apha          see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param tile.alpha         see 'Arguments' in the function \code{\link{na.pattern}}.
#' @param violin.alpha       see 'Arguments' in the function \code{\link{test.levene}}.
#' @param violin.trim        see 'Arguments' in the function \code{\link{test.levene}}.
#' @param box.width          see 'Arguments' in the function \code{\link{test.levene}}.
#' @param box.alpha          see 'Arguments' in the function \code{\link{test.levene}}.
#' @param linetype           see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}})
#'                           or \code{\link{test.t}}.
#' @param linewidth          see 'Arguments' in the function \code{\link{test.t}}.
#' @param line.col           see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param intercept          see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param density.col        see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param density.linewidth  see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param density.linetype   see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param point.size         see 'Arguments' in the functions \code{\link{aov.b}},
#'                           \code{\link{aov.w}}, \code{ci.*} (e.g., \code{\link{ci.cor}}), \code{\link{test.t}},
#'                           or \code{\link{test.welch}}.
#' @param point.linewidth    see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param point.linetype     see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param point.shape        see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param point.col          see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param ci.col             see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param ci.linewidth       see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param ci.linetype        see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param errorbar.width     see 'Arguments' in the functions \code{\link{aov.b}},
#'                           \code{\link{aov.w}}, \code{ci.*} (e.g., \code{\link{ci.cor}}), \code{\link{test.t}},
#'                           or \code{\link{test.welch}},
#' @param dodge.width        see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param jitter.size        see 'Arguments' in the functions \code{\link{aov.b}},
#'                           \code{\link{aov.w}}, \code{\link{test.levene}},
#'                           \code{\link{test.t}}, or \code{\link{test.welch}}.
#' @param jitter.width       see 'Arguments' in the functions \code{\link{aov.b}},
#'                           \code{\link{aov.w}}, \code{\link{test.levene}},
#'                           \code{\link{test.t}}, or \code{\link{test.welch}}.
#' @param jitter.height      see 'Arguments' in the functions \code{\link{aov.b}},
#'                           \code{\link{test.levene}}, \code{\link{test.t}}, or
#'                           \code{\link{test.welch}}.
#' @param jitter.alpha       see 'Arguments' in the functions \code{\link{aov.b}},
#'                           \code{\link{aov.w}}, \code{\link{test.levene}},
#'                           \code{\link{test.t}}, or \code{\link{test.welch}}.
#' @param gray               see 'Arguments' in the functions \code{\link{multilevel.r2}}
#'                           or \code{\link{test.levene}}.
#' @param start              see 'Arguments' in the functions \code{\link{multilevel.r2}}
#'                           or \code{\link{test.levene}}.
#' @param end                see 'Arguments' in the functions \code{\link{multilevel.r2}}
#'                           or \code{\link{test.levene}}.
#' @param color              see 'Arguments' in the functions \code{\link{multilevel.r2}},
#'                           \code{\link{na.pattern}}, or \code{\link{test.levene}}.
#' @param xlab               see 'Arguments' in the functions \code{\link{aov.b}},
#'                           \code{\link{aov.w}}, \code{ci.*} (e.g., \code{\link{ci.cor}}),
#'                           \code{\link{test.levene}}, \code{\link{test.t}}, or \code{\link{test.welch}}.
#' @param ylab               see 'Arguments' in the functions \code{\link{aov.b}},
#'                           \code{\link{aov.w}}, \code{ci.*} (e.g., \code{\link{ci.cor}}),
#'                           \code{\link{test.levene}}, \code{\link{test.t}}, or \code{\link{test.welch}}.
#' @param xlim               see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}),
#'                           \code{\link{test.levene}}, \code{\link{test.t}}, or
#'                           \code{\link{test.welch}}.
#' @param ylim               see 'Arguments' in the functions \code{\link{aov.b}},
#'                           \code{\link{aov.w}}, \code{ci.*} (e.g., \code{\link{ci.cor}}),
#'                           \code{\link{test.levene}}, \code{\link{test.t}}, or \code{\link{test.welch}}.
#' @param xbreaks            see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}),
#'                           \code{\link{test.levene}}, \code{\link{test.t}}, or
#'                           \code{\link{test.welch}}.
#' @param ybreaks            see 'Arguments' in the functions \code{\link{aov.b}},
#'                           \code{\link{aov.w}}, \code{ci.*} (e.g., \code{\link{ci.cor}}),
#'                           \code{\link{test.levene}}, \code{\link{test.t}}, or \code{\link{test.welch}}.
#' @param axis.title.size    see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param axis.text.size     see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param strip.text.size    see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param title              see 'Arguments' in the functions \code{\link{aov.b}},
#'                           \code{\link{aov.w}}, \code{ci.*} (e.g., \code{\link{ci.cor}}),
#'                           \code{\link{test.levene}}, \code{\link{test.t}}, or \code{\link{test.welch}}.
#' @param subtitle           see 'Arguments' in the functions \code{\link{aov.b}},
#'                           \code{\link{aov.w}}, \code{ci.*} (e.g., \code{\link{ci.cor}}),
#'                           \code{\link{test.levene}}, \code{\link{test.t}}, or \code{\link{test.welch}}.
#' @param group.col          see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param plot.margin        see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param legend.title       see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param legend.position    see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param legend.box.margin  see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}})
#'                           or \code{\link{na.pattern}}.
#' @param legend.key.size    see 'Arguments' in the function \code{\link{na.pattern}}.
#' @param legend.text.size   see 'Arguments' in the functions \code{\link{na.pattern}}.
#' @param facet.ncol         see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param facet.nrow         see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param facet.scales       see 'Arguments' in the functions \code{ci.*} (e.g., \code{\link{ci.cor}}).
#' @param filename           see 'Arguments' in the functions \code{\link{aov.b}},
#'                           \code{\link{aov.w}}, \code{ci.*} (e.g., \code{\link{ci.cor}}),
#'                           \code{\link{test.levene}}, \code{\link{test.t}}, or \code{\link{test.welch}}.
#' @param width              see 'Arguments' in the functions \code{\link{aov.b}},
#'                           \code{\link{aov.w}}, \code{ci.*} (e.g., \code{\link{ci.cor}}),
#'                           \code{\link{test.levene}}, \code{\link{test.t}}, or \code{\link{test.welch}}.
#' @param height             see 'Arguments' in the functions \code{\link{aov.b}},
#'                           \code{\link{aov.w}}, \code{ci.*} (e.g., \code{\link{ci.cor}}),
#'                           \code{\link{test.levene}}, \code{\link{test.t}}, or \code{\link{test.welch}}.
#' @param units              see 'Arguments' in the functions \code{\link{aov.b}},
#'                           \code{\link{aov.w}}, \code{ci.*} (e.g., \code{\link{ci.cor}}),
#'                           \code{\link{test.levene}}, \code{\link{test.t}}, or \code{\link{test.welch}}.
#' @param dpi                see 'Arguments' in the functions \code{\link{aov.b}},
#'                           \code{\link{aov.w}}, \code{ci.*} (e.g., \code{\link{ci.cor}}),
#'                           \code{\link{test.levene}}, \code{\link{test.t}}, or \code{\link{test.welch}}.
#' @param check              logical: if \code{TRUE} (default), argument specification
#'                           is checked.
#' @param ...                further arguments passed to or from other methods.
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

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a misty object for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  # Check if input 'x' is a misty object
  if (isTRUE(!inherits(x, "misty.object"))) { stop("Please specify a misty object for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is supported by the function
  if (isTRUE(!x$type %in% c("aov.b", "aov.w", "ci.cor", "ci.mean", "ci.median", "ci.prop", "ci.var", "ci.sd", "multilevel.r2", "multilevel.r2.manual", "na.pattern", "test.levene", "test.welch", "test.z", "test.t"))) { stop("This type of misty object is not supported by the function.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("bar", "box", "violin", "hist", "point", "line", "ci", "jitter", "density", "square", "rotate"), envir = environment(), input.check = check)

  # Global Variables
  group <- id <- low <- m <- m.diff <- m.low <- m.upp <- mu <- n.pattern <- obs_miss <- part <- pattern <- upp <- variable <- NULL

  #_____________________________________________________________________________
  #
  # Between-Subject Analysis of Variance (ANOVA) -------------------------------
  switch(x$type, "aov.b" = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Compute Means and (Difference-Adjusted) Confidence Intervals by Group ####

    ci.table <- misty::ci.mean(x$data, y, group = "group", adjust = adjust, conf.level = conf.level, output = FALSE)$result

    #...................
    ### Create ggplot ####

    # Subtitle
    if (isTRUE(ci)) { if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% ", ifelse(isTRUE(adjust), "Difference-Adjusted ", ""), "Confidence Interval") } else { subtitle } } else { subtitle <- "" }

    p <- ggplot2::ggplot(x$data, ggplot2::aes(group, y)) +
      ggplot2::scale_x_discrete(name = xlab) +
      ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = title, subtitle = subtitle) +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5), plot.title = ggplot2::element_text(hjust = 0.5))

    #...................
    ### Bars ####

    if (isTRUE(bar)) { p <- p + suppressWarnings(ggplot2::geom_bar(data = ci.table, ggplot2::aes(group, m), stat = "summary", fun = "mean")) }

    #...................
    ### Confidence Intervals ####

    if (isTRUE(ci)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(data = ci.table, ggplot2::aes(group, m, ymin = low, ymax = upp), width = errorbar.width)) }

    #...................
    ### Points ####

    if (isTRUE(point)) { p <- p + suppressWarnings(ggplot2::geom_point(data = ci.table, ggplot2::aes(group, m), stat = "identity", size = point.size)) }

    #...................
    ### Jittered Points ####

    if (isTRUE(jitter)) { p <- p + suppressWarnings(ggplot2::geom_jitter(alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

  #_____________________________________________________________________________
  #
  # Repeated Measures Analysis of Variance (Within-Subject ANOVA) --------------
  }, "aov.w" = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Compute Means and (Difference-Adjusted) Confidence Intervals by Group ####

    ci.table <- misty::ci.mean.w(x$data$wide, adjust = adjust, conf.level = conf.level, na.omit = x$args$na.omit, check = FALSE, output = FALSE)$result

    #...................
    ### Create ggplot ####

    # Subtitle
    if (isTRUE(ci)) { if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% ", ifelse(isTRUE(adjust), "Difference-Adjusted ", ""), "Cousineau-Morey Confidence Interval") } else { subtitle } } else { subtitle <- "" }

    p <- ggplot2::ggplot(x$data$long, ggplot2::aes(time, y, group = 1L)) +
      ggplot2::scale_x_discrete(name = xlab) +
      ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = title, subtitle = subtitle) +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5), plot.title = ggplot2::element_text(hjust = 0.5))

    #...................
    ### Confidence Intervals ####

    if (isTRUE(ci)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(data = ci.table, ggplot2::aes(variable, m, ymin = low, ymax = upp), width = errorbar.width)) }

    #...................
    ### Points ####

    if (isTRUE(point)) { p <- p + suppressWarnings(ggplot2::geom_point(data = ci.table, ggplot2::aes(variable, m), stat = "identity", size = point.size)) }

    #...................
    ### Lines ####

    if (isTRUE(line)) { p <- p + suppressWarnings(ggplot2::geom_line(data = ci.table, ggplot2::aes(variable, m), stat = "identity", linewidth = linewidth)) }

    #...................
    ### Add jittered points ####

    if (isTRUE(jitter)) { p <- p + ggplot2::geom_point(data = x$data$long, ggplot2::aes(time, y, group = id), alpha = jitter.alpha, position = ggplot2::position_dodge(jitter.width)) }

    #...................
    ### Individual lines ####

    if (isTRUE(jitter && line)) { p <- p + ggplot2::geom_line(data = x$data$long, ggplot2::aes(time, y, group = id), alpha = jitter.alpha, position = ggplot2::position_dodge(jitter.width)) }

  #_____________________________________________________________________________
  #
  # (Bootstrap) Confidence Intervals for Correlation Coefficients --------------
  }, "ci.cor" = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Correlation Coefficient with Error Bars ####

    switch(plot, "ci" = {

      if (isTRUE(any(!is.na((if (isTRUE(!is.data.frame(x$result))) { do.call("rbind", x$result) } else { x$result })$low)))) {

        p <- .plot.ci(result = x$result, stat = "cor", group = x$data$group, split = x$data$split, point.size = point.size, point.shape = point.shape, errorbar.width = errorbar.width, dodge.width = dodge.width, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        stop("There are no confidence intervals for the correlation coefficient to plot.", call. = FALSE)

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bootstrap Samples ####

    }, "boot" = {

      if (any(!is.na(x$boot$cor))) {

        p <- .plot.boot(result = x$result, boot.sample = x$boot, stat = "cor", group = x$data$group, split = x$data$split, hist = hist, binwidth = binwidth, bins = bins, alpha = hist.alpha, fill = fill, density = density, density.col = density.col, density.linewidth = density.linewidth, density.linetype = density.linetype, plot.point = point, point.col = point.col, point.linewidth = point.linewidth, point.linetype = point.linetype, plot.ci = ci, ci.col = ci.col, ci.linewidth = ci.linewidth, ci.linetype = ci.linetype, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        stop("There are no bootstrap samples to plot.", call. = FALSE)

      }

    })

  #_____________________________________________________________________________
  #
  # (Bootstrap) Confidence Intervals for Arithmetic Means ----------------------
  }, "ci.mean" = {

    switch(plot, "ci" = {

      if (isTRUE(any(!is.na((if (isTRUE(!is.data.frame(x$result))) { do.call("rbind", x$result) } else { x$result })$low)))) {

        p <- .plot.ci(result = x$result, stat = "m", group = x$data$group, split = x$data$split, point.size = point.size, point.shape = point.shape, errorbar.width = errorbar.width, dodge.width = dodge.width, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        plot <- "none"

        warning("There are no confidence intervals for the arithmetic mean to plot.", call. = FALSE)

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bootstrap Samples ####

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

    #_____________________________________________________________________________
    #
    # Plot -----------------------------------------------------------------------

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Median with Error Bars ####

    switch(plot, "ci" = {

      if (isTRUE(any(!is.na((if (isTRUE(!is.data.frame(x$result))) { do.call("rbind", x$result) } else { x$result })$low)))) {

        p <- .plot.ci(result = x$result, stat = "med", group = x$data$group, split = x$data$split, point.size = point.size, point.shape = point.shape, errorbar.width = errorbar.width, dodge.width = dodge.width, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        warning("There are no confidence intervals for the median to plot.", call. = FALSE)

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bootstrap Samples ####

    }, "boot" = {

      if (any(!is.na(x$boot$med))) {

        p <- .plot.boot(result = x$result, boot.sample = x$boot, stat = "med", group = x$data$group, split = x$data$split, hist = hist, binwidth = binwidth, bins = bins, alpha = hist.alpha, fill = fill, density = density, density.col = density.col, density.linewidth = density.linewidth, density.linetype = density.linetype, plot.point = point, point.col = point.col, point.linewidth = point.linewidth, point.linetype = point.linetype, plot.ci = ci, ci.col = ci.col, ci.linewidth = ci.linewidth, ci.linetype = ci.linetype, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        warning("There are no bootstrap samples to plot.", call. = FALSE)

      }

    })

  #_____________________________________________________________________________
  #
  # (Bootstrap) Confidence Intervals for Proportions ----------------------------
  }, "ci.prop" = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Proportion with Error Bars ####

    switch(plot, "ci" = {

      if (isTRUE(any(!is.na((if (isTRUE(!is.data.frame(x$result))) { do.call("rbind", x$result) } else { x$result })$low)))) {

        p <- .plot.ci(result = x$result, stat = "prop", group = x$data$group, split = x$data$split, point.size = point.size, point.shape = point.shape, errorbar.width = errorbar.width, dodge.width = dodge.width, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)$p

      } else {

        warning("There are no confidence intervals for the proportion to plot.", call. = FALSE)

      }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

  #___________________________________________________________________________
  #
  # R-Squared Measures for Multilevel and LMMs -------------------------------
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

  #___________________________________________________________________________
  #
  # R-Squared Measures for Multilevel and LMMS, Manual Input -----------------
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

  #___________________________________________________________________________
  #
  # Missing Data Pattern -----------------------------------------------------
  }, "na.pattern" = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

  #___________________________________________________________________________
  #
  # Levene's Test for Homogeneity of Variance --------------------------------
  }, "test.levene" = {

    #...................
    ### Create plot ####

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

  #___________________________________________________________________________
  #
  # t-Test -------------------------------------------------------------------
  }, "test.t" = {

    switch(x$sample,
           #...................
           ### One-sample ####
           "one" = {

             #...................
             ### Create ggplot ####

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
             ### Bars ####

             if (isTRUE(bar)) { p <- p + suppressWarnings(ggplot2::geom_bar(data = x$result, ggplot2::aes(0, m), stat = "summary", fun = "mean")) }

             #...................
             ### Point ####

             if (isTRUE(point)) { p <- p + suppressWarnings(ggplot2::geom_point(data = x$result, ggplot2::aes(0, m), stat = "identity", size = point.size)) }

             #...................
             ### Confidence Intervals ####

             if (isTRUE(ci)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(data = x$result, ggplot2::aes(0, m, ymin = m.low, ymax = m.upp), width = errorbar.width)) }

             #...................
             ### Jittered Points ####

             if (isTRUE(jitter)) { p <- p + suppressWarnings(ggplot2::geom_jitter(data = data.frame(x = x$data$x), ggplot2::aes(x = 0L, y = x), alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

             #...................
             ### Horizontal Line ####

             if (isTRUE(line)) { p <- p + ggplot2::geom_hline(data = data.frame(mu = x$args$mu), ggplot2::aes(yintercept = mu), linetype = linetype, linewidth = linewidth) }

           #...................
           ### Two-sample ####
           }, "two" = {

             # Plot data
             plotdat <- split(x$data[, 1L], f = x$data[, 2L]) |> (\(y) data.frame(group = factor(c(rep(x$result[1L, "group"], times = length(y[[1L]])), rep(x$result[2L, "group"], times = length(y[[2L]])))), y = unlist(y)) )()

             # Confidence interval
             ci.table <- misty::ci.mean(plotdat[, "y"], group = plotdat[, "group"], adjust = adjust, conf.level = conf.level, output = FALSE)$result

             #...................
             ### Create ggplot ####

             # Subtitle
             if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% ", ifelse(isTRUE(adjust), "Difference-Adjusted ", ""), "Confidence Interval") } else { subtitle }

             p <- ggplot2::ggplot(ci.table, ggplot2::aes(group, m), stat = "identity", size = point.size) +
               ggplot2::scale_x_discrete(name = xlab) +
               ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
               ggplot2::labs(title = title, subtitle = subtitle) + ggplot2::theme_bw() +
               ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5), plot.title = ggplot2::element_text(hjust = 0.5))

             #...................
             ### Bars ####

             if (isTRUE(bar)) { p <- p + suppressWarnings(ggplot2::geom_bar(ggplot2::aes(group, m), stat = "summary", fun = "mean")) }

             #...................
             ### Point ####

             if (isTRUE(point)) { p <- p + suppressWarnings(ggplot2::geom_point(ggplot2::aes(group, m), stat = "identity", size = point.size)) }

             #...................
             ### Confidence Intervals ####

             if (isTRUE(ci)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(ggplot2::aes(group, m, ymin = low, ymax = upp), width = errorbar.width)) }

             #...................
             ### Jittered Points ####

             if (isTRUE(jitter)) { p <- p + suppressWarnings(ggplot2::geom_jitter(data = plotdat, ggplot2::aes(x = group, y = y), alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

           #...................
           ### Paired-sample ####
           }, "paired" = {

             #...................
             ### Create ggplot ####

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
             ### Bars ####

             if (isTRUE(bar)) { p <- p + suppressWarnings(ggplot2::geom_bar(data = x$result, ggplot2::aes(0, m.diff), stat = "summary", fun = "mean")) }

             #...................
             ### Point ####

             if (isTRUE(point)) { p <- p + suppressWarnings(ggplot2::geom_point(data = x$result, ggplot2::aes(0, m.diff), stat = "identity", size = point.size)) }

             #...................
             ### Confidence Intervals ####

             if (isTRUE(ci)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(data = x$result, ggplot2::aes(0, m.diff, ymin = m.low, ymax = m.upp), width = errorbar.width)) }

             #...................
             ### Jittered Points ####

             if (isTRUE(jitter)) { p <- p + suppressWarnings(ggplot2::geom_jitter(data = data.frame(x = x$data$y - x$data$x), ggplot2::aes(x = 0L, y = x), alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

             #...................
             ### Horizontal Line ####

             if (isTRUE(line)) { p <- p + ggplot2::geom_hline(yintercept = 0L, linetype = linetype, linewidth = linewidth) }

           })

  #___________________________________________________________________________
  #
  # Welch's Test -------------------------------------------------------------
  }, test.welch = {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Compute Means and (Difference-Adjusted) Confidence Intervals by Group ####

    ci.table <- misty::ci.mean(x$data, y, group = "group", adjust = adjust, conf.level = conf.level, output = FALSE)$result

    #...................
    ### Create ggplot ####

    # Subtitle
    if (isTRUE(ci)) { if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% ", ifelse(isTRUE(adjust), "Difference-Adjusted ", ""), "Confidence Interval") } else { subtitle } } else { subtitle <- "" }

    p <- ggplot2::ggplot(x$data, ggplot2::aes(group, y)) +
      ggplot2::scale_x_discrete(name = xlab) +
      ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = title, subtitle = subtitle) +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5), plot.title = ggplot2::element_text(hjust = 0.5))

    #...................
    ### Bars ####

    if (isTRUE(bar)) { p <- p + suppressWarnings(ggplot2::geom_bar(data = ci.table, ggplot2::aes(group, m), stat = "summary", fun = "mean")) }

    #...................
    ### Confidence Intervals ####

    if (isTRUE(ci)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(data = ci.table, ggplot2::aes(group, m, ymin = low, ymax = upp), width = errorbar.width)) }

    #...................
    ### Points ####

    if (isTRUE(point)) { p <- p + suppressWarnings(ggplot2::geom_point(data = ci.table, ggplot2::aes(group, m), stat = "identity", size = point.size)) }

    #...................
    ### Jittered Points ####

    if (isTRUE(jitter)) { p <- p + suppressWarnings(ggplot2::geom_jitter(data = x$data, ggplot2::aes(group, y), alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

  #___________________________________________________________________________
  #
  # z-Test -------------------------------------------------------------------
  }, "test.z" = {

    switch(x$sample,

           #...................
           ### One-sample ####
           "one" = {

             #...................
             ### Create ggplot ####

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
             ### Bars ####

             if (isTRUE(bar)) { p <- p + suppressWarnings(ggplot2::geom_bar(data = x$result, ggplot2::aes(0, m), stat = "summary", fun = "mean")) }

             #...................
             ### Point ####

             if (isTRUE(point)) { p <- p + suppressWarnings(ggplot2::geom_point(data = x$result, ggplot2::aes(0, m), stat = "identity", size = point.size)) }

             #...................
             ### Confidence Intervals ####

             if (isTRUE(ci)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(data = x$result, ggplot2::aes(0, m, ymin = m.low, ymax = m.upp), width = errorbar.width)) }

             #...................
             ### Jittered Points ####

             if (isTRUE(jitter)) { p <- p + suppressWarnings(ggplot2::geom_jitter(data = data.frame(x = x$data$x), ggplot2::aes(x = 0L, y = x), alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

             #...................
             ### Horizontal Line ####

             if (isTRUE(line)) { p <- p + ggplot2::geom_hline(data = data.frame(mu = x$args$mu), ggplot2::aes(yintercept = mu), linetype = linetype, linewidth = linewidth) }

           #...................
           ### Two-sample ####
           }, "two" = {

             # Plot data
             plotdat <- split(x$data[, 1L], f = x$data[, 2L]) |> (\(y) data.frame(group = factor(c(rep(x$result[1L, "group"], times = length(y[[1L]])), rep(x$result[2L, "group"], times = length(y[[2L]])))), y = unlist(y)))()

             # Confidence interval
             ci.table <- misty::ci.mean(plotdat[, "y"], group = plotdat[, "group"], adjust = adjust, conf.level = conf.level, output = FALSE)$result

             #...................
             ### Create ggplot ####

             # Subtitle
             if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% ", ifelse(isTRUE(adjust), "Difference-Adjusted ", ""), "Confidence Interval") } else { subtitle }

             p <- ggplot2::ggplot(ci.table, ggplot2::aes(group, m), stat = "identity", size = point.size) +
               ggplot2::scale_x_discrete(name = xlab) +
               ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = ybreaks) +
               ggplot2::labs(title = title, subtitle = subtitle) + ggplot2::theme_bw() +
               ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5), plot.title = ggplot2::element_text(hjust = 0.5))

             #...................
             ### Bars ####

             if (isTRUE(bar)) { p <- p + suppressWarnings(ggplot2::geom_bar(ggplot2::aes(group, m), stat = "summary", fun = "mean")) }

             #...................
             ### Point ####

             if (isTRUE(point)) { p <- p + suppressWarnings(ggplot2::geom_point(ggplot2::aes(group, m), stat = "identity", size = point.size)) }

             #...................
             ### Confidence Intervals ####

             if (isTRUE(ci)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(ggplot2::aes(group, m, ymin = low, ymax = upp), width = errorbar.width)) }

             #...................
             ### Jittered Points ####

             if (isTRUE(jitter)) { p <- p + suppressWarnings(ggplot2::geom_jitter(data = plotdat, ggplot2::aes(x = group, y = y), alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

           #...................
           ### Paired-sample ####
           }, "paired" = {

             #...................
             ### Create ggplot ####

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
             ### Bars ####

             if (isTRUE(bar)) { p <- p + suppressWarnings(ggplot2::geom_bar(data = x$result, ggplot2::aes(0, m.diff), stat = "summary", fun = "mean")) }

             #...................
             ### Point ####

             if (isTRUE(point)) { p <- p + suppressWarnings(ggplot2::geom_point(data = x$result, ggplot2::aes(0, m.diff), stat = "identity", size = point.size)) }

             #...................
             ### Confidence Intervals ####

             if (isTRUE(ci)) { p <- p + suppressWarnings(ggplot2::geom_errorbar(data = x$result, ggplot2::aes(0, m.diff, ymin = m.low, ymax = m.upp), width = errorbar.width)) }

             #...................
             ### Jittered Points ####

             if (isTRUE(jitter)) { p <- p + suppressWarnings(ggplot2::geom_jitter(data = data.frame(x = x$data$y - x$data$x), ggplot2::aes(x = 0L, y = x), alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size)) }

             #...................
             ### Horizontal Line ####

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
