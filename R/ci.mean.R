#' (Bootstrap) Confidence Intervals for Arithmetic Means and Medians
#'
#' The function \code{ci.mean} computes and plots confidence intervals for
#' arithmetic means with known or unknown population standard deviation or
#' population variance and the function \code{ci.median} computes confidence
#' intervals for medians, optionally by a grouping and/or split variable. These
#' functions also supports six types of bootstrap confidence intervals (e.g.,
#' bias-corrected (BC) percentile bootstrap or bias-corrected and accelerated
#' (BCa) bootstrap confidence intervals) and plots the bootstrap samples with
#' histograms and density curves.
#'
#' @param ...               a numeric vector, matrix or data frame with numeric
#'                          variables, i.e., factors and character variables are
#'                          excluded from \code{x} before conducting the analysis.
#'                          Alternatively, an expression indicating the variable
#'                          names in \code{data} e.g., \code{ci.mean(x1, x2, data = dat)}.
#'                          Note that the operators \code{.}, \code{+}, \code{-},
#'                          \code{~}, \code{:}, \code{::}, and \code{!} can also
#'                          be used to select variables, see 'Details' in the
#'                          \code{\link{df.subset}} function.
#' @param data              a data frame when specifying one or more variables
#'                          in the argument \code{...}. Note that the argument
#'                          is \code{NULL} when specifying a numeric vector,
#'                          matrix or data frame for the argument \code{...}.
#' @param sigma             a numeric vector indicating the population standard
#'                          deviation when computing confidence intervals for the
#'                          arithmetic mean with known standard deviation Note
#'                          that either argument \code{sigma} or argument
#'                          \code{sigma2} is specified and it is only possible to
#'                          specify one value for the argument \code{sigma} even
#'                          though multiple variables are specified in \code{x}.
#' @param sigma2            a numeric vector indicating the population variance
#'                          when computing confidence intervals for the arithmetic
#'                          mean with known variance. Note that either argument
#'                          \code{sigma} or argument \code{sigma2} is specified
#'                          and it is only possible to specify one value for the
#'                          argument \code{sigma2} even though multiple variables
#'                          are specified in \code{x}.
#' @param adjust            logical: if \code{TRUE}, difference-adjustment for
#'                          the confidence intervals for the arithmetic mean
#'                          (Baguley, 2012) is applied.
#' @param boot              a character string specifying the type of bootstrap
#'                          confidence intervals (CI), i.e., \code{"none"}
#'                          (default) for not conducting bootstrapping, \code{"norm"}
#'                          for the bias-corrected normal approximation bootstrap CI,
#'                          \code{"basic"} for the basic bootstrap CI, \code{"stud"}
#'                          for the studentized bootstrap CI, \code{"perc"}, for
#'                          the percentile bootstrap CI \code{"bc"} for the bias-corrected
#'                          (BC) percentile bootstrap CI (without acceleration),
#'                          and \code{"bca"} for the bias-corrected and accelerated
#'                          (BCa) bootstrap CI, see 'Details' in the
#'                          \code{\link{ci.cor}} function.
#' @param R                 a numeric value indicating the number of bootstrap
#'                          replicates (default is 1000).
#' @param seed              a numeric value specifying seeds of the pseudo-random
#'                          numbers used in the bootstrap algorithm when conducting
#'                          bootstrapping.
#' @param sample            logical: if \code{TRUE} (default), the univariate
#'                          sample skewness and kurtosis is computed, while the
#'                          population skewness and kurtosis is computed when
#'                          \code{sample = FALSE}.
#' @param alternative       a character string specifying the alternative hypothesis,
#'                          must be one of \code{"two.sided"} (default),
#'                          \code{"greater"} or \code{"less"}.
#' @param conf.level        a numeric value between 0 and 1 indicating the confidence
#'                          level of the interval.
#' @param group             either a character string indicating the variable name
#'                          of the grouping variable in \code{...} or \code{data},
#'                          or a vector representing the grouping variable. Note
#'                          that a grouping variable can only be used when computing
#'                          confidence intervals with unknown population standard
#'                          deviation and population variance.
#' @param split             either a character string indicating the variable name
#'                          of the split variable in \code{...} or \code{data},
#'                          or a vector representing the split variable. Note
#'                          that a grouping variable can only be used when computing
#'                          confidence intervals with unknown population standard
#'                          deviation and population variance.
#' @param sort.var          logical: if \code{TRUE}, output table is sorted by
#'                          variables when specifying \code{group}.
#' @param na.omit           logical: if \code{TRUE}, incomplete cases are removed
#'                          before conducting the analysis (i.e., listwise deletion)
#'                          when specifying more than one outcome variable.
#' @param digits            an integer value indicating the number of decimal
#'                          places to be used.
#' @param as.na             a numeric vector indicating user-defined missing
#'                          values, i.e. these values are converted to \code{NA}
#'                          before conducting the analysis. Note that \code{as.na()}
#'                          function is only applied to \code{x}, but not to
#'                          \code{group} or \code{split}.
#' @param plot              a character string indicating the type of the plot
#'                          to display, i.e., \code{"none"} (default) for not
#'                          displaying any plots, \code{"ci"} for displaying
#'                          confidence intervals for the arithmetic mean or median,
#'                          \code{"boot"} for displaying bootstrap samples with
#'                          histograms and density curves when the argument
#'                          \code{"boot"} is other than \code{"none"}.
#' @param point.size        a numeric value indicating the \code{size} argument
#'                          in the \code{geom_point} function for controlling the
#'                          size of points when plotting confidence intervals
#'                          (\code{plot = "ci"}).
#' @param point.shape       a numeric value between 0 and 25 or a character string
#'                          as plotting symbol indicating the \code{shape} argument
#'                          in the \code{geom_point} function for controlling the
#'                          symbols of points when plotting confidence intervals
#'                          (\code{plot = "ci"}).
#' @param errorbar.width    a numeric value indicating the \code{width} argument
#'                          in the \code{geom_errorbar} function for controlling
#'                          the width of the whiskers in the \code{geom_errorbar}
#'                          function when plotting confidence intervals
#'                          (\code{plot = "ci"}).
#' @param dodge.width       a numeric value indicating the \code{width} argument
#'                          controlling the width of the \code{geom} elements to
#'                          be dodged when specifying a grouping variable using
#'                          the argument \code{group} and plotting confidence
#'                          intervals (\code{plot = "ci"}).
#' @param hist              logical: if \code{TRUE} (default), histograms are
#'                          drawn when plotting bootstrap samples
#'                          (\code{plot = "boot"}).
#' @param binwidth          a numeric value or a function for specifying the
#'                          \code{binwidth} argument in the \code{geom_histogram}
#'                          function for controlling the width of the bins when
#'                          plotting bootstrap samples (\code{plot = "boot"}).
#' @param bins              a numeric value for specifying the \code{bins} argument
#'                          in the \code{geom_histogram} function for controlling
#'                          the number of bins when plotting bootstrap samples
#'                          (\code{plot = "boot"}).
#' @param alpha             a numeric value between 0 and 1 for specifying the
#'                          \code{alpha} argument in the \code{geom_histogram}
#'                          function for controlling the opacity of the bars
#'                          when plotting bootstrap samples (\code{plot = "boot"}).
#' @param fill              a character string specifying the \code{fill} argument
#'                          in the \code{geom_histogram} function controlling the
#'                          fill aesthetic when plotting bootstrap samples
#'                          (\code{plot = "boot"}). Note that this argument applied
#'                          only when no grouping variable was specified
#'                          \code{group = NULL}.
#' @param density           logical: if \code{TRUE} (default), density curves are
#'                          drawn when plotting bootstrap samples (\code{plot = "boot"}).
#' @param density.col       a character string specifying the \code{color} argument
#'                          in the \code{geom_density} function controlling the
#'                          color of the density curves when plotting bootstrap
#'                          samples (\code{plot = "boot"}). Note that this argument
#'                          applied only when no grouping variable was specified
#'                          \code{group = NULL}.
#' @param density.linewidth a numeric value specifying the \code{linewidth}
#'                          argument in the \code{geom_density} function controlling
#'                          the line width of the density curves when plotting
#'                          bootstrap samples (\code{plot = "boot"}).
#' @param density.linetype  a numeric value or character string specifying the
#'                          \code{linetype} argument in the \code{geom_density}
#'                          function controlling the line type of the density
#'                          curves when plotting bootstrap samples
#'                          (\code{plot = "boot"}).
#' @param plot.point        logical: if \code{TRUE} (default), vertical lines
#'                          representing the point estimate of the arithmetic
#'                          mean or median are drawn when plotting bootstrap samples
#'                          (\code{plot = "boot"}).
#' @param point.col         a character string specifying the \code{color} argument
#'                          in the \code{geom_vline} function for controlling the
#'                          color of the vertical line displaying the arithmetic
#'                          mean or median when plotting bootstrap samples
#'                          (\code{plot = "boot"}). Note that this argument
#'                          applied only when no grouping variable was specified
#'                          \code{group = NULL}.
#' @param point.linewidth   a numeric value specifying the \code{linewdith}
#'                          argument in the \code{geom_vline} function for
#'                          controlling the line width of the vertical line
#'                          displaying the arithmetic mean or median when plotting
#'                          bootstrap samples (\code{plot = "boot"}).
#' @param point.linetype    a numeric value or character string specifying the
#'                          \code{linetype} argument in the \code{geom_vline}
#'                          function controlling the line type of the vertical
#'                          line displaying the arithmetic mean or median when
#'                          plotting bootstrap samples (\code{plot = "boot"}).
#' @param plot.ci           logical: if \code{TRUE} (default), vertical lines
#'                          representing the bootstrap confidence intervals of
#'                          the arithmetic mean or median are drawn when
#'                          plotting bootstrap samples (\code{plot = "boot"}).
#' @param ci.col            character string specifying the \code{color} argument
#'                          in the \code{geom_vline} function for controlling the
#'                          color of the vertical line displaying bootstrap
#'                          confidence intervals when plotting bootstrap samples
#'                          (\code{plot = "boot"}). Note that this argument applied
#'                          only when no grouping variable was specified
#'                          \code{group = NULL}.
#' @param ci.linewidth      a numeric value specifying the \code{linewdith} argument
#'                          in the \code{geom_vline} function for controlling the
#'                          line width of the vertical line displaying bootstrap
#'                          confidence intervals when plotting bootstrap samples
#'                          (\code{plot = "boot"}).
#' @param ci.linetype       a numeric value or character string specifying the
#'                          \code{linetype} argument in the \code{geom_vline}
#'                          function controlling the line type of the vertical
#'                          line displaying bootstrap confidence intervals when
#'                          plotting bootstrap samples (\code{plot = "boot"}).
#' @param line              logical: if \code{TRUE}, a horizontal line
#'                          is drawn when \code{plot = "ci"} or a vertical line
#'                          is drawn when \code{plot = "boot"}
#' @param intercept         a numeric value indicating the \code{yintercept} or
#'                          \code{xintercept} argument in the \code{geom_hline}
#'                          or \code{geom_vline} function controlling the position
#'                          of the horizontal or vertical line when \code{plot = "ci"}
#'                          and \code{line = TRUE} or when \code{plot = "boot"}
#'                          and \code{line = TRUE}. By default, the horizontal or
#'                          vertical line is drawn at 0.
#' @param linetype          a character string indicating the \code{linetype}
#'                          argument in the \code{geom_hline} or \code{geom_vline}
#'                          function controlling the line type of the horizontal
#'                          or vertical line (default is \code{linetype = "dashed"}).
#' @param line.col          a character string indicating the \code{color} argument
#'                          in the \code{geom_hline} or \code{geom_vline} function
#'                          for controlling the color of the horizontal or vertical
#'                          line.
#' @param xlab              a character string indicating the \code{name} argument
#'                          in the \code{scale_x_continuous} function for labeling
#'                          the x-axis. The default setting is \code{xlab = NULL}
#'                          when \code{plot = "ci"} and \code{xlab = "Arithmetic Mean"}
#'                          or \code{xlab = "Median"} when \code{plot = "boot"}.
#' @param ylab              a character string indicating the \code{name} argument
#'                          in the \code{scale_y_continuous} function for labeling
#'                          the y-axis. The default setting is \code{ylab = "Arithmetic Mean"}
#'                          or \code{ylab = "Median"} when \code{plot = "ci"} and
#'                          \code{ylab = "Probability Density, f(x)"} when \code{plot = "boot"}.
#' @param xlim              a numeric vector with two elements indicating the
#'                          \code{limits} argument in the \code{scale_x_continuous}
#'                          function for controlling the scale range of the x-axis.#'
#' @param ylim              a numeric vector with two elements indicating the
#'                          \code{limits} argument in the \code{scale_y_continuous}
#'                          function for controlling the scale range of the y-axis.
#' @param xbreaks           a numeric vector indicating the \code{breaks} argument
#'                          in the \code{scale_x_continuous} function for controlling
#'                          the x-axis breaks. The default setting is
#'                          \code{xbreaks = NULL} when \code{plot = "ci"}
#'                          and \code{xbreaks = seq(-1, 1, by = 0.25)} when
#'                          \code{plot = "boot"}.
#' @param ybreaks           a numeric vector indicating the \code{breaks} argument
#'                          in the \code{scale_y_continuous} function for controlling
#'                          the y-axis breaks. The default setting is
#'                          \code{ybreaks = seq(-1, 1, by = 0.25)} when
#'                          \code{plot = "ci"} and \code{ybreaks = NULL} when
#'                          \code{plot = "boot"}.
#' @param axis.title.size   a numeric value indicating the \code{size} argument
#'                          in the \code{element_text} function for specifying the
#'                          function controlling the font size of the axis title,
#'                          i.e., \code{theme(axis.title = element_text(size = axis.text.size))}.
#' @param axis.text.size    a numeric value indicating the \code{size} argument
#'                          in the \code{element_text} function for specifying the
#'                          function controlling the font size of the axis text,
#'                          i.e., \code{theme(axis.text = element_text(size = axis.text.size))}.
#' @param strip.text.size   a numeric value indicating the \code{size} argument
#'                          in the \code{element_text} function for specifying the
#'                          function controlling the font size of the strip text,
#'                          i.e., \code{theme(strip.text = element_text(size = strip.text.size))}.
#' @param title             a character string indicating the \code{title} argument
#'                          in the \code{labs} function for the subtitle of the plot.
#' @param subtitle          a character string indicating the \code{subtite} argument
#'                          in the \code{labs} function for the subtitle of the plot.
#' @param group.col         a character vector indicating the \code{color} argument
#'                          in the \code{scale_color_manual} and \code{scale_fill_manual}
#'                          functions when specifying a grouping variable using
#'                          the argument \code{group}.
#' @param plot.margin       a numeric vector with four elements indicating the
#'                          \code{plot.margin} argument in the \code{theme} function
#'                          controlling the plot margins . The default setting
#'                          is \code{c(5.5, 5.5, 5.5, 5.5)}, but switches
#'                          to \code{c(5.5, 5.5, -2.5, 5.5)} when specifying a
#'                          grouping variable using the argument \code{group}.
#' @param legend.title      a character string indicating the \code{color} argument
#'                          in the \code{labs} function for specifying the legend
#'                          title when specifying a grouping variable using the
#'                          argument \code{group}.
#' @param legend.position   a character string indicating the \code{legend.position}
#'                          in the \code{theme} argument for controlling the
#'                          position of the legend  function when specifying a
#'                          grouping variable using the argument \code{group}.
#'                          By default, the legend is placed at the bottom the
#'                          plot.
#' @param legend.box.margin a numeric vector with four elements indicating the
#'                          \code{legend.box.margin} argument in the \code{theme}
#'                          function for controlling the margins around the full
#'                          legend area when specifying a grouping variable using
#'                          the argument \code{group}.
#' @param facet.ncol        a numeric value indicating the \code{ncol} argument
#'                          in the \code{facet_wrap} function for controlling
#'                          the number of columns when specifying a split variable
#'                          using the argument \code{split}.
#' @param facet.nrow        a numeric value indicating the \code{nrow} argument
#'                          in the \code{facet_wrap} function for controlling the
#'                          number of rows when specifying a split variable using
#'                          the argument \code{split}.
#' @param facet.scales      a character string indicating the \code{scales} argument
#'                          in the \code{facet_wrap} function for controlling the
#'                          scales shared across facets, i.e., \code{"fixed"},
#'                          \code{"free_x"}, \code{"free_y"}, or \code{"free"}
#'                          (default) when specifying a split variable using
#'                          the argument \code{split}.
#' @param saveplot          a character string indicating the \code{filename}
#'                          argument including the file extension in the \code{ggsave}
#'                          function. Note that one of \code{".eps"}, \code{".ps"},
#'                          \code{".tex"}, \code{".pdf"} (default),
#'                          \code{".jpeg"}, \code{".tiff"}, \code{".png"},
#'                          \code{".bmp"}, \code{".svg"} or \code{".wmf"} needs
#'                          to be specified as file extension in the \code{file}
#'                          argument. Note that plots can only be saved when
#'                          \code{plot = "ci"} or \code{plot = "boot"}.
#' @param width             a numeric value indicating the \code{width} argument
#'                          (default is the size of the current graphics device)
#'                          in the \code{ggsave} function.
#' @param height            a numeric value indicating the \code{height} argument
#'                          (default is the size of the current graphics device)
#'                          in the \code{ggsave} function.
#' @param units             a character string indicating the \code{units} argument
#'                          (default is \code{in}) in the \code{ggsave} function.
#' @param dpi               a numeric value indicating the \code{dpi} argument
#'                          (default is \code{600}) in the \code{ggsave} function.
#' @param write             a character string naming a file for writing the output
#'                          into either a text file with file extension \code{".txt"}
#'                          (e.g., \code{"Output.txt"}) or Excel file with file
#'                          extension \code{".xlsx"}  (e.g., \code{"Output.xlsx"}).
#'                          If the file name does not contain any file extension,
#'                          an Excel file will be written.
#' @param append            logical: if \code{TRUE} (default), output will be
#'                          appended to an existing text file with extension
#'                          \code{.txt} specified in \code{write}, if \code{FALSE}
#'                          existing text file will be overwritten.
#' @param check             logical: if \code{TRUE} (default), argument specification
#'                          is checked.
#' @param output            logical: if \code{TRUE} (default), output is shown
#'                          on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @name ci.mean
#'
#' @seealso
#' \code{\link{test.z}}, \code{\link{test.t}}, \code{\link{ci.mean.diff}},
#' \code{\link{ci.cor}}, \code{\link{ci.prop}}, \code{\link{ci.var}},
#' \code{\link{ci.sd}}, \code{\link{descript}}
#'
#' @references
#' Baguley, T. S. (2012). \emph{Serious stats: A guide to advanced statistics for
#' the behavioral sciences}. Palgrave Macmillan.
#'
#' Canty, A., & Ripley, B. (2024). \emph{boot: Bootstrap R (S-Plus) Functions}.
#' R package version 1.3-31.
#'
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
#' - Using R and SPSS}. John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{list with the input specified in \code{...}, \code{data}, \code{group}, and \code{split}}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{boot}}{data frame with bootstrap replicates of the arithmetic mean of median when bootstrapping was requested}
#' \item{\code{plot}}{ggplot2 object for plotting the results and the data frame used for plotting}
#' \item{\code{result}}{result table}
#'
#' @note
#' Bootstrap confidence intervals are computed using the R package \code{boot}
#' by Angelo Canty and Brain Ripley (2024).
#'
#' @export
#'
#' @examples
#' #----------------------------------------------------------------------------
#' # Example 1: Confidence Interval (CI) for the Arithmetic Mean
#'
#' # Example 1a: Two-Sided 95% CI
#' ci.mean(mtcars)
#'
#' # Alternative specification
#' ci.mean(., data = mtcars)
#'
#' # Example 1b: Two-Sided 95% Difference-Adjusted CI
#' ci.mean(mtcars, adjust = TRUE)
#'
#' # Example 1c: Two-Sided 95% CI with known population standard deviation
#' ci.mean(mtcars$mpg, sigma = 6)
#'
#' # Alternative specification
#' ci.mean(mpg, data = mtcars, sigma = 6)
#'
#' #----------------------------------------------------------------------------
#' # Example 2: Confidence Interval (CI) for the Median
#'
#' # Example 2a: Two-Sided 95% CI
#' ci.median(mtcars)
#'
#' # Alternative specification
#' ci.median(., data = mtcars)
#'
#' # Example 2b: One-Sided 99% CI
#' ci.median(mtcars, alternative = "less", conf.level = 0.99)
#'
#' \dontrun{
#' #----------------------------------------------------------------------------
#' # Example 3: Bootstrap Confidence Interval (CI)
#'
#' # Example 3a: Bias-corrected (BC) percentile bootstrap CI
#' ci.mean(mtcars, boot = "bc")
#'
#' # Example 3b: Bias-corrected and accelerated (BCa) bootstrap CI,
#' # 5000 bootstrap replications, set seed of the pseudo-random number generator
#' ci.mean(mtcars, boot = "bca", R = 5000, seed = 123)
#'
#' #----------------------------------------------------------------------------
#' # Example 4: Grouping and Split Variable
#'
#' # Example 4a: Grouping variable
#' ci.mean(mpg, cyl, disp, data = mtcars, group = "vs")
#'
#' # Alternative specification
#' ci.mean(mtcars[, c("mpg", "cyl", "disp")], group = mtcars$vs)
#'
#' # Example 4b: Split variable
#' ci.mean(mpg, cyl, disp, data = mtcars, split = "am")
#'
#' # Alternative specification
#' ci.mean(mtcars[, c("mpg", "cyl", "disp")], split = mtcars$am)
#'
#' # Example 4c: Grouping and split variable
#' ci.mean(mpg, cyl, disp, data = mtcars, group = "vs", split = "am")
#'
#' # Alternative specification
#' ci.mean(mtcars[, c("mpg", "cyl", "disp")], group = mtcars$vs, split = mtcars$am)
#'
#' #----------------------------------------------------------------------------
#' # Example 5: Write Output
#'
#' # Example 5a: Text file
#' ci.mean(mtcars, write = "CI_Mean_Text.txt")
#'
#' # Example 5b: Excel file
#' ci.mean(mtcars, write = "CI_Mean_Excel.xlsx")
#'
#' #----------------------------------------------------------------------------
#' # Example 6: Plot Confidence Intervals
#'
#' # Example 6a: Two-Sided 95% CI
#' ci.mean(disp, hp, data = mtcars, plot = "ci")
#'
#' # Example 6b: Grouping variable
#' ci.mean(disp, hp, data = mtcars, group = "vs", plot = "ci")
#'
#' # Example 6c: Split variable
#' ci.mean(disp, hp, data = mtcars, split = "am", plot = "ci")
#'
#' # Example 6d: Save plot as PDF file
#' ci.mean(disp, hp, data = mtcars, plot = "ci", saveplot = "CI_Mean.pdf",
#'         width = 9, height = 6)
#'
#' # Example 6e: Save plot as PNG file
#' ci.mean(disp, hp, data = mtcars, plot = "ci", saveplot = "CI_Mean.png",
#'         width = 9, height = 6)
#'
#' #----------------------------------------------------------------------------
#' # Example 7: Plot Bootstrap Samples
#'
#' # Example 7a: Two-Sided 95% CI
#' ci.mean(disp, hp, data = mtcars, boot = "bc", plot = "boot")
#'
#' # Example 7b: Grouping variable
#' ci.mean(disp, hp, data = mtcars, group = "vs", boot = "bc", plot = "boot")
#'
#' # Example 7c: Split variable
#' ci.mean(disp, hp, data = mtcars, split = "am", boot = "bc", plot = "boot")
#'
#' # Example 7d: Save plot as PDF file
#' ci.mean(disp, hp, data = mtcars, boot = "bc", plot = "boot",
#'         saveplot = "CI_Mean_Boot.pdf", width = 12, height = 7)
#'
#' # Example 7e: Save plot as PNG file
#' ci.mean(disp, hp, data = mtcars, boot = "bc", plot = "boot",
#'         saveplot = "CI_Mean_Boot.png", width = 12, height = 7)
#' }
ci.mean <- function(..., data = NULL, sigma = NULL, sigma2 = NULL, adjust = FALSE,
                    boot = c("none", "norm", "basic", "stud", "perc", "bc", "bca"),
                    R = 1000, seed = NULL, sample = TRUE,
                    alternative = c("two.sided", "less", "greater"),
                    conf.level = 0.95, group = NULL, split = NULL, sort.var = FALSE,
                    na.omit = FALSE, digits = 2, as.na = NULL,
                    plot = c("none", "ci", "boot"), point.size = 2.5, point.shape = 19,
                    errorbar.width = 0.3, dodge.width = 0.5, hist = TRUE,
                    binwidth = NULL, bins = NULL, alpha = 0.4, fill = "gray85", density = TRUE,
                    density.col = "#0072B2", density.linewidth = 0.5, density.linetype = "solid",
                    plot.point = TRUE, point.col = "#CC79A7", point.linewidth = 0.6,
                    point.linetype = "solid", plot.ci = TRUE, ci.col = "black",
                    ci.linewidth = 0.6, ci.linetype = "dashed", line = FALSE, intercept = 0,
                    linetype = "solid", line.col = "gray65", xlab = NULL, ylab = NULL,
                    xlim = NULL, ylim = NULL, xbreaks = ggplot2::waiver(), ybreaks = ggplot2::waiver(),
                    axis.title.size = 11, axis.text.size = 10, strip.text.size = 11, title = NULL,
                    subtitle = NULL, group.col = NULL, plot.margin = NA, legend.title = "",
                    legend.position = c("right", "top", "left", "bottom", "none"),
                    legend.box.margin = c(-10, 0, 0, 0), facet.ncol = NULL, facet.nrow = NULL,
                    facet.scales = "free", saveplot = NULL, width = NA, height = NA,
                    units = c("in", "cm", "mm", "px"), dpi = 600, write = NULL,
                    append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input '...' is missing
  if (isTRUE(missing(...))) { stop("Please specify the argument '...'.", call. = FALSE) }

  # Check if input '...' is NULL
  if (isTRUE(is.null(substitute(...)))) { stop("Input specified for the argument '...' is NULL.", call. = FALSE) }

  # Check if input 'data' is data frame
  if (isTRUE(!is.null(data) && !is.data.frame(data))) { stop("Please specify a data frame for the argument 'data'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!is.null(data))) {

    # Convert tibble into data frame
    if (isTRUE("tbl" %in% substr(class(data), 1L, 3L))) { data <- as.data.frame(data) }

    # Extract variables
    x <- data[, .var.names(..., data = data, group = group, split = split, check.chr = "a numeric vector, matrix or data frame"), drop = FALSE]

    # Grouping variable
    if (isTRUE(!is.null(group))) { group <- data[, group] }

    # Splitting variable
    if (isTRUE(!is.null(split))) { split <- data[, split] }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

    # Convert tibble into data frame
    if (isTRUE("tbl" %in% substr(class(x), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(x)) == 1L)) { x <- unlist(x) } else { x <- as.data.frame(x) } }
    if (isTRUE("tbl" %in% substr(class(group), 1L, 3L))) { group <- unlist(group) }
    if (isTRUE("tbl" %in% substr(class(split), 1L, 3L))) { group <- unlist(split) }

    # Data and cluster
    var.group <- .var.group(data = x, group = group, split = split)

    # Data
    if (isTRUE(!is.null(var.group$data)))  { x <- var.group$data }

    # Grouping variable
    if (isTRUE(!is.null(var.group$group))) { group <- var.group$group }

    # Split variable
    if (isTRUE(!is.null(var.group$split))) { split <- var.group$split }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data Frame ####

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Numeric Variables ####

  x <- x |> (\(y) !vapply(y, is.numeric, FUN.VALUE = logical(1L)))() |> (\(z) if (isTRUE(any(z))) {

    warning(paste0("Non-numeric variables were excluded from the analysis: ", paste(names(which(z)), collapse = ", ")), call. = FALSE)

    return(x[, -which(z), drop = FALSE])

  } else {

    return(x)

  })()

  if (isTRUE(ncol(x) == 0L)) { stop("No variables left for analysis after excluding non-numeric variables.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping and Split Variable ####

  # Grouping variable
  if (!is.null(group)) {

    x <- which(sapply(names(x), function(y) identical(group, x[, y]))) |> (\(z) if (isTRUE(length(z) != 0L)) { return(x[, -z]) } else { x })()

    if (isTRUE(ncol(x) == 0L)) { stop("After excluding the grouping variable from the matrix or data frame, there are no variables left.") }

  }

  # Split variable
  if (!is.null(split)) {

    x <- which(sapply(names(x), function(y) identical(split, x[, y]))) |> (\(z) if (isTRUE(length(z) != 0L)) { return(x[, -z]) } else { x })()

    if (isTRUE(ncol(x) == 0L)) { stop("After excluding the split variable from the matrix or data frame, there are no variables left.") }

  }

  # Grouping and split variable are identical
  if (isTRUE(!is.null(group) && !is.null(split) && identical(group, split))) { stop("Grouping and split variables are identical.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise deletion ####

  # Check input 'na.omit'
  .check.input(logical = "na.omit", envir = environment(), input.check = check)

  if (isTRUE(na.omit && any(is.na(x)))) {

    assign("x", na.omit(x)) |> (\(y) warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(y)$na.action)), call. = FALSE))()

    # Grouping variable
    if (isTRUE(!is.null(group))) { group <- group[-attributes(na.omit(x))$na.action] }

    # Split variable
    if (isTRUE(!is.null(split))) { split <- split[-attributes(na.omit(x))$na.action] }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("adjust", "sample", "sort.var", "na.omit", "plot.point", "plot.ci", "line", "append", "output"),
               numeric = list(seed = 1L, point.size = 1L, point.shape  = 1L, errorbar.width = 1L, dodge.width = 1L, bins = 1L, density.linewidth = 1L, point.linewidth = 1L, ci.linewidth = 1L, intercept = 1L, xlim = 2L, ylim = 2L, axis.title.size = 1L, axis.text.size = 1L, strip.text.size = 1L, plot.margin = 4L, legend.box.margin = 4L, facet.ncol = 1L, facet.nrow = 1L, width = 1L, height = 1L, dpi = 1L),
               character = list(title = 1L, subtitle = 1L, legend.title = 1L),
               s.character = list(boot = c("none", "norm", "basic", "stud", "perc", "bc", "bca"), plot = c("none", "ci", "boot")),
               args = c("R", "alternative", "conf.level", "digits", "alpha", "linetype", "units", "legend.position", "facet.scales", "write2"), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(isTRUE(check))) {

    # Check input 'sigma' and 'sigma2'
    if (isTRUE(!is.null(sigma) && !is.null(sigma2))) { stop("Please specify either argument 'sigma' or argument 'sigma2', but not both.", call. = FALSE) }

    # Check input 'sigma'
    if (isTRUE(!is.null(sigma) && (sigma <= 0L || length(sigma) != 1L))) { stop("Please specify a numeric value grater than 0 for the argument 'sigma'.", call. = FALSE) }

    # Check input 'sigma2'
    if (isTRUE(!is.null(sigma2) && (sigma2 <= 0L || length(sigma2) != 1L))) { stop("Please specify a numeric value grater than 0 for the argument 'sigma2'.", call. = FALSE) }

    if (isTRUE(!"none" %in% boot)) {

      if (isTRUE(!is.null(sigma))) { stop("Confidence intervals with known population standard deviation are not supported when using bootstrapping.", call. = FALSE) }

      if (isTRUE(!is.null(sigma2))) { stop("Confidence intervals with known population variance are not supported when using bootstrapping.", call. = FALSE) }

       if (isTRUE(adjust)) { stop("Difference-adjustment is not supported for bootstrap confidence intervals.", call. = FALSE) }

    }

    # Check input 'group'
    if (isTRUE(!is.null(group))) {

      # Population standard deviation
      if (isTRUE(!is.null(sigma))) { stop("Grouping variable cannot be used for confidence intervals with known population standard deviation.", call. = FALSE) }

      # Population variance
      if (isTRUE(!is.null(sigma2))) { stop("Grouping variable cannot be used for confidence intervals with known population variance.", call. = FALSE) }

      # Input 'group' completely missing
      if (isTRUE(all(is.na(group)))) { stop("The grouping variable specified in 'group' is completely missing.", call. = FALSE) }

      # Only one group in 'group'
      if (isTRUE(length(na.omit(unique(unlist(group)))) == 1L)) { warning("There is only one group represented in the grouping variable specified in 'group'.", call. = FALSE) }

    }

    # Check input 'split'
    if (isTRUE(!is.null(split))) {

      # Population standard deviation
      if (isTRUE(!is.null(sigma))) { stop("Split variable cannot be used for confidence intervals with known population standard deviation.", call. = FALSE) }

      # Population variance
      if (isTRUE(!is.null(sigma2))) { stop("Split variable cannot be used for confidence intervals with known population variance.", call. = FALSE) }

      # Input 'split' completely missing
      if (isTRUE(all(is.na(split)))) { stop("The split variable specified in 'split' is completely missing.", call. = FALSE) }

      # Only one group in 'split'
      if (isTRUE(length(na.omit(unique(split))) == 1L)) { warning("There is only one group represented in the split variable specified in 'split'.", call. = FALSE) }

    }

    # Check input 'plot'
    if (isTRUE(all(plot == "boot") && (all(boot == "none") || all(c("none", "norm", "basic", "stud", "perc", "bc", "bca") %in% boot)))) { stop("Please request bootstrap confidence intervals by specifying the 'boot' argument to plot bootstrap samples.", call. = FALSE) }

    # Check input 'group.col'
    if (isTRUE(!is.null(group.col) && length(group.col) != length(unique(group)))) { stop(paste0("Please specify a character vector with ", length(unique(group)), " elements for the argument 'group.col'."), call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Population standard deviation and variance ####

  if (isTRUE(is.null(sigma) && !is.null(sigma2))) { sigma <- sqrt(sigma2) }

  if (isTRUE(!is.null(sigma) && is.null(sigma2))) { sigma2 <- sigma^2 }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'boot' Argument ####

  boot <- ifelse(all(c("none", "norm", "basic", "stud", "perc", "bc", "bca") %in% boot), "none", boot)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Alternative hypothesis ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'plot' Argument ####

  plot <- ifelse(all(c("none", "ci", "boot") %in% plot), "none", plot)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'xlab' and 'ylab', Argument ####

  switch(plot, "ci" = {

    ylab <- if (isTRUE(is.null(ylab))) { "Arithmetic Mean" } else { ylab }

  }, "boot" = {

    xlab <- if (isTRUE(is.null(xlab))) { "Arithmetic Mean" } else { xlab }
    ylab <- if (isTRUE(is.null(ylab))) { "Probability Density, f(x)" } else { ylab }

  })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'plot.margin' Argument ####

  if (isTRUE(is.na(plot.margin))) { if (isTRUE(is.null(group))) { plot.margin <- c(5.5, 5.5, 5.5, 5.5) } else { plot.margin <- c(5.5, 5.5, -2.5, 5.5) } }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'legend.position' Argument ####

  if (isTRUE(all(c("right", "top", "left", "bottom", "none") %in% legend.position))) { legend.position  <- "bottom" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'units' Argument ####

  # Default setting
  if (isTRUE(all(c("in", "cm", "mm", "px") %in% units))) { units <- "in" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, No Split ####

  if (isTRUE(is.null(group) && is.null(split))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No Bootstrapping ####

    if (isTRUE(boot == "none")) {

      result <- data.frame(variable = colnames(x),
                           n = vapply(x, function(y) length(na.omit(y)), FUN.VALUE = 1L),
                           nNA = vapply(x, function(y) sum(is.na(y)), FUN.VALUE = 1L),
                           pNA = vapply(x, function(y) sum(is.na(y)) / length(y) * 100, FUN.VALUE = double(1L)),
                           # Standard deviation
                           sd = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, sd(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                           # Skewness
                           skew = vapply(x, function(y) ifelse(length(na.omit(y)) <= 3L, NA, suppressWarnings(misty::skewness(y, sample = sample, check = FALSE))), FUN.VALUE = double(1L)),
                           # Kurtosis
                           kurt = vapply(x, function(y) ifelse(length(na.omit(y)) <= 4L, NA, suppressWarnings(misty::kurtosis(y, sample = sample, check = FALSE))), FUN.VALUE = double(1L)),
                           # Arithmetic mean
                           m = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, mean(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                           # Confidence interval for the arithmetic mean
                           low = vapply(x, .m.conf, sigma = sigma, adjust = adjust, alternative = alternative, conf.level = conf.level, side = "low", FUN.VALUE = double(1L)),
                           upp = vapply(x, .m.conf, sigma = sigma, adjust = adjust, alternative = alternative, conf.level = conf.level, side = "upp", FUN.VALUE = double(1L)),
                           row.names = NULL, check.names = FALSE)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bootstrapping ####

    } else {

      result.boot <- apply(matrix(seq_len(ncol(x)), nrow = 1L), 2L, function(y) x |>
                             (\(z) suppressWarnings(.ci.boot(data = x[, y], statistic = .boot.func.mean, boot = boot, R = R, alternative = alternative, conf.level = conf.level, seed = seed)))() |>
                             (\(w) list(t = data.frame(variable = colnames(x)[y], m = w$t), result = data.frame(variable = colnames(x)[y], n = w$n, nNA = w$nNA, pNA = w$pNA, sd = w$sd, skew = w$skew, kurt = w$kurt, m = w$t0[1L], low = w$ci[1L], upp = w$ci[2L], row.names = NULL)))())

      boot.sample <- do.call("rbind", lapply(result.boot, function(y) y$t))
      result <- do.call("rbind", lapply(result.boot, function(y) y$result))

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, No Split ####

  } else if (isTRUE(!is.null(group) && is.null(split))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No Bootstrapping ####

    if (isTRUE(boot == "none")) {

      result <- lapply(split(x, f = group), function(y) misty::ci.mean(y, data = NULL, group = NULL, split = NULL, adjust = adjust, sample = sample, alternative = alternative, conf.level = conf.level, sort.var = sort.var, check = FALSE, output = FALSE)$result) |>
        (\(y) data.frame(group = rep(names(y), each = ncol(x)), eval(parse(text = paste0("rbind(", paste0("y[[", seq_len(length(y)), "]]", collapse = ", "), ")")))) )()

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bootstrapping ####

    } else {

      result.boot <- lapply(split(x, f = group), function(y) misty::ci.mean(y, data = NULL, group = NULL, split = NULL, sample = sample, seed = seed, boot = boot, R = R, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)) |>
        (\(z) list(boot.sample = data.frame(group = rep(names(z), each = R*unique(unlist(lapply(z, function(q) nrow(q$result))))), do.call("rbind", lapply(z, function(w) w$boot)), row.names = NULL),
                   result = data.frame(group = rep(names(z), each = unique(unlist(lapply(z, function(q) nrow(q$result))))), do.call("rbind", lapply(z, function(w) w$result)), row.names = NULL)))()

      boot.sample <- result.boot$boot.sample
      result <- result.boot$result

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, Split ####

  } else if (isTRUE(is.null(group) && !is.null(split))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No Bootstrapping ####

    if (isTRUE(boot == "none")) {

      result <- lapply(split(data.frame(x), f = split), function(y) misty::ci.mean(y, data = NULL, group = NULL, split = NULL, adjust = adjust, sample = sample, alternative = alternative, conf.level = conf.level, sort.var = sort.var, check = FALSE, output = FALSE)$result)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bootstrapping ####

    } else {

      result.boot <- lapply(split(x, f = split), function(y) misty::ci.mean(y, data = NULL, group = NULL, split = NULL, sample = sample, seed = seed, boot = boot, R = R, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)) |>
        (\(z) list(boot.sample = data.frame(split = rep(names(z), each = R*unique(unlist(lapply(z, function(q) nrow(q$result))))), do.call("rbind", lapply(z, function(w) w$boot)), row.names = NULL), result = lapply(z, function(w) w$result)))()

      boot.sample <- result.boot$boot.sample
      result <- result.boot$result

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, Split ####

  } else if (isTRUE(!is.null(group) && !is.null(split))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No Bootstrapping ####

    if (isTRUE(boot == "none")) {

      result <- lapply(split(data.frame(x, group = group), f = split), function(y) misty::ci.mean(y[, -grep("group", names(y))], data = NULL, group = y$group, split = NULL, adjust = adjust, alternative = alternative, conf.level = conf.level, sort.var = sort.var, check = FALSE, output = FALSE)$result)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bootstrapping ####

    } else {

      result.boot <- lapply(split(data.frame(x, group = group), f = split), function(y) misty::ci.mean(y[, -grep("group", names(y))], data = NULL, group = y$group, split = NULL, sample = sample, seed = seed, boot = boot, R = R, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)) |>
        (\(z) list(boot.sample = data.frame(split = rep(names(z), each = R*unique(unlist(lapply(z, function(q) nrow(q$result))))), do.call("rbind", lapply(z, function(w) w$boot)), row.names = NULL), result = lapply(z, function(w) w$result)))()

      boot.sample <- result.boot$boot.sample
      result <- result.boot$result

    }

  }

  #_____________________________________________________________________________
  #
  # Plot -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Arithmetic Mean with Error Bars ####

  switch(plot, "ci" = {

    if (isTRUE(any(!is.na((if (isTRUE(!is.data.frame(result))) { do.call("rbind", result) } else { result })$low)))) {

      plot.object <- .plot.ci(result = result, stat = "m", group = group, split = split, point.size = point.size, point.shape = point.shape, errorbar.width = errorbar.width, dodge.width = dodge.width, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)

      #...................
      ### Print plot ####

      suppressWarnings(print(plot.object$p))

    } else {

      plot <- "none"

      warning("There are no confidence intervals for the arithmetic mean to plot.", call. = FALSE)

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Bootstrap Samples ####

  }, "boot" = {

    if (any(!is.na(boot.sample$m))) {

      plot.object <- .plot.boot(result = result, boot.sample = boot.sample, stat = "m", group = group, split = split, hist = hist, binwidth = binwidth, bins = bins, alpha = alpha, fill = fill, density = density, density.col = density.col, density.linewidth = density.linewidth, density.linetype = density.linetype, plot.point = plot.point, point.col = point.col, point.linewidth = point.linewidth, point.linetype = point.linetype, plot.ci = plot.ci, ci.col = ci.col, ci.linewidth = ci.linewidth, ci.linetype = ci.linetype, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)

      #...................
      ### Print plot ####

      suppressMessages(suppressWarnings(print(plot.object$p)))

    } else {

      plot <- "none"

      warning("There are no bootstrap samples to plot.", call. = FALSE)

    }

  })

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "ci.mean",
                 data = list(x = x, group = group, split = split),
                 args = list(sigma = sigma, sigma2 = sigma2, adjust = adjust, boot = boot, R = R, seed = seed, sample = sample, alternative = alternative, conf.level = conf.level, sort.var = sort.var, na.omit = na.omit, digits = digits, as.na = as.na, plot = plot, point.size = point.size, point.shape = point.shape, errorbar.width = errorbar.width, dodge.width = dodge.width, hist = hist, binwidth = binwidth, bins = bins, alpha = alpha, fill = fill, density = density, density.col = density.col, density.linewidth = density.linewidth, density.linetype = density.linetype, plot.point = plot.point, point.col = point.col, point.linewidth = point.linewidth, point.linetype = point.linetype, plot.ci = plot.ci, ci.col = ci.col, ci.linewidth = ci.linewidth, ci.linetype = ci.linetype, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales, saveplot = saveplot, width = width, height = height, units = units, dpi = dpi, write = write, append = append, check = check, output = output),
                 boot = if (isTRUE(boot != "none")) { boot.sample } else { NULL },
                 plot = if (isTRUE(plot != "none")) { list(plot = plot.object$p, plotdata = plot.object$plotdat) } else { NULL },
                 result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) { .write.result(object = object, write = write, append = append) }

  #_____________________________________________________________________________
  #
  # Save ggplot ----------------------------------------------------------------

  if (isTRUE(plot != "none" && !is.null(saveplot))) { suppressWarnings(suppressMessages(ggplot2::ggsave(filename = saveplot, plot = plot.object$p, width = width, height = height, units = units, dpi = dpi))) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
#_______________________________________________________________________________

#' @rdname ci.median
ci.median <- function(..., data = NULL, boot = c("none", "norm", "basic", "stud", "perc", "bc", "bca"),
                      R = 1000, seed = NULL, sample = TRUE,
                      alternative = c("two.sided", "less", "greater"),
                      conf.level = 0.95, group = NULL, split = NULL, sort.var = FALSE,
                      na.omit = FALSE, digits = 2, as.na = NULL, plot = c("none", "ci", "boot"),
                      point.size = 2.5, point.shape = 19, errorbar.width = 0.3, dodge.width = 0.5,
                      hist = TRUE, binwidth = NULL, bins = NULL, alpha = 0.4, fill = "gray85",
                      density = TRUE, density.col = "#0072B2", density.linewidth = 0.5,
                      density.linetype = "solid", plot.point = TRUE, point.col = "#CC79A7",
                      point.linewidth = 0.6, point.linetype = "solid", plot.ci = TRUE, ci.col = "black",
                      ci.linewidth = 0.6, ci.linetype = "dashed", line = FALSE, intercept = 0,
                      linetype = "solid", line.col = "gray65", xlab = NULL, ylab = NULL,
                      xlim = NULL, ylim = NULL, xbreaks = ggplot2::waiver(), ybreaks = ggplot2::waiver(),
                      axis.title.size = 11, axis.text.size = 10, strip.text.size = 11, title = NULL,
                      subtitle = NULL, group.col = NULL, plot.margin = NA,  legend.title = "",
                      legend.position = c("right", "top", "left", "bottom", "none"),
                      legend.box.margin = c(-10, 0, 0, 0), facet.ncol = NULL, facet.nrow = NULL,
                      facet.scales = "free", saveplot = NULL, width = NA, height = NA,
                      units = c("in", "cm", "mm", "px"), dpi = 600, write = NULL, append = TRUE,
                      check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input '...' is missing
  if (isTRUE(missing(...))) { stop("Please specify the argument '...'.", call. = FALSE) }

  # Check if input '...' is NULL
  if (isTRUE(is.null(substitute(...)))) { stop("Input specified for the argument '...' is NULL.", call. = FALSE) }

  # Check if input 'data' is data frame
  if (isTRUE(!is.null(data) && !is.data.frame(data))) { stop("Please specify a data frame for the argument 'data'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!is.null(data))) {

    # Convert tibble into data frame
    if (isTRUE("tbl" %in% substr(class(data), 1L, 3L))) { data <- as.data.frame(data) }

    # Extract variables
    x <- data[, .var.names(..., data = data, group = group, split = split, check.chr = "a numeric vector, matrix or data frame"), drop = FALSE]

    # Grouping variable
    if (isTRUE(!is.null(group))) { group <- data[, group] }

    # Splitting variable
    if (isTRUE(!is.null(split))) { split <- data[, split] }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

    # Convert tibble into data frame
    if (isTRUE("tbl" %in% substr(class(x), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(x)) == 1L)) { x <- unlist(x) } else { x <- as.data.frame(x) } }
    if (isTRUE("tbl" %in% substr(class(group), 1L, 3L))) { group <- unlist(group) }
    if (isTRUE("tbl" %in% substr(class(split), 1L, 3L))) { group <- unlist(split) }

    # Data and cluster
    var.group <- .var.group(data = x, group = group, split = split)

    # Data
    if (isTRUE(!is.null(var.group$data)))  { x <- var.group$data }

    # Grouping variable
    if (isTRUE(!is.null(var.group$group))) { group <- var.group$group }

    # Split variable
    if (isTRUE(!is.null(var.group$split))) { split <- var.group$split }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data Frame ####

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Numeric Variables ####

  x <- x |> (\(y) !vapply(y, is.numeric, FUN.VALUE = logical(1L)))() |> (\(z) if (isTRUE(any(z))) {

    warning(paste0("Non-numeric variables were excluded from the analysis: ", paste(names(which(z)), collapse = ", ")), call. = FALSE)

    return(x[, -which(z), drop = FALSE])

  } else {

    return(x)

  })()

  if (isTRUE(ncol(x) == 0L)) { stop("No variables left for analysis after excluding non-numeric variables.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping and Split Variable ####

  # Grouping variable
  if (!is.null(group)) {

    x <- which(sapply(names(x), function(y) identical(group, x[, y]))) |> (\(z) if (isTRUE(length(z) != 0L)) { return(x[, -z]) } else { x })()

    if (isTRUE(ncol(x) == 0L)) { stop("After excluding the grouping variable from the matrix or data frame, there are no variables left.") }

  }

  # Split variable
  if (!is.null(split)) {

    x <- which(sapply(names(x), function(y) identical(split, x[, y]))) |> (\(z) if (isTRUE(length(z) != 0L)) { return(x[, -z]) } else { x })()

    if (isTRUE(ncol(x) == 0L)) { stop("After excluding the split variable from the matrix or data frame, there are no variables left.") }

  }

  # Grouping and split variable are identical
  if (isTRUE(!is.null(group) && !is.null(split) && identical(group, split))) { stop("Grouping and split variables are identical.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise deletion ####

  # Check input 'na.omit'
  .check.input(logical = "na.omit", envir = environment(), input.check = check)

  if (isTRUE(na.omit && any(is.na(x)))) {

    assign("x", na.omit(x)) |> (\(y) warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(y)$na.action)), call. = FALSE))()

    # Grouping variable
    if (isTRUE(!is.null(group))) { group <- group[-attributes(na.omit(x))$na.action] }

    # Split variable
    if (isTRUE(!is.null(split))) { split <- split[-attributes(na.omit(x))$na.action] }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("sample", "sort.var", "na.omit", "plot.point", "plot.ci", "line", "append", "output"),
               numeric = list(seed = 1L, point.size = 1L, point.shape  = 1L, errorbar.width = 1L, dodge.width = 1L, bins = 1L, density.linewidth = 1L, point.linewidth = 1L, ci.linewidth = 1L, intercept = 1L, xlim = 2L, ylim = 2L, axis.title.size = 1L, axis.text.size = 1L, strip.text.size = 1L, plot.margin = 4L, legend.box.margin = 4L, facet.ncol = 1L, facet.nrow = 1L, width = 1L, height = 1L, dpi = 1L),
               character = list(title = 1L, subtitle = 1L, legend.title = 1L),
               s.character = list(boot = c("none", "norm", "basic", "stud", "perc", "bc", "bca"), plot = c("none", "ci", "boot")),
               args = c("R", "alternative", "conf.level", "digits", "alpha", "linetype", "units", "legend.position", "facet.scales", "write2"), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check input 'plot'
    if (isTRUE(all(plot == "boot") && boot == "none")) { stop("Please request bootstrap confidence intervals by specifying the 'boot' argument to plot bootstrap samples.", call. = FALSE) }

    # Check input 'group.col'
    if (isTRUE(!is.null(group.col) && length(group.col) != length(unique(group)))) { stop(paste0("Please specify a character vector with ", length(unique(group)), " elements for the argument 'group.col'."), call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'boot' Argument ####

  boot <- ifelse(all(c("none", "norm", "basic", "stud", "perc", "bc", "bca") %in% boot), "none", boot)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Alternative hypothesis ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'plot' Argument ####

  plot <- ifelse(all(c("none", "ci", "boot") %in% plot), "none", plot)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'xlab' and 'ylab', Argument ####

  switch(plot, "ci" = {

    ylab <- if (isTRUE(is.null(ylab))) { "Median" } else { ylab }

  }, "boot" = {

    xlab <- if (isTRUE(is.null(xlab))) { "Median" } else { xlab }
    ylab <- if (isTRUE(is.null(ylab))) { "Probability Density, f(x)" } else { ylab }

  })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'plot.margin' Argument ####

  if (isTRUE(is.na(plot.margin))) { if (isTRUE(is.null(group))) { plot.margin <- c(5.5, 5.5, 5.5, 5.5) } else { plot.margin <- c(5.5, 5.5, -2.5, 5.5) } }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'legend.position' Argument ####

  if (isTRUE(all(c("right", "top", "left", "bottom", "none") %in% legend.position))) { legend.position  <- "bottom" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'units' Argument ####

  # Default setting
  if (isTRUE(all(c("in", "cm", "mm", "px") %in% units))) { units <- "in" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, No Split ####

  if (isTRUE(is.null(group) && is.null(split))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No Bootstrapping ####

    if (isTRUE(boot == "none")) {

      result <- data.frame(variable = colnames(x),
                           n = vapply(x, function(y) length(na.omit(y)), FUN.VALUE = 1L),
                           nNA = vapply(x, function(y) sum(is.na(y)), FUN.VALUE = 1L),
                           pNA = vapply(x, function(y) sum(is.na(y)) / length(y) * 100L, FUN.VALUE = double(1)),
                           # Standard deviation
                           sd = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, sd(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                           # Interquartile range
                           iqr = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, IQR(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                           # Skewness
                           skew = vapply(x, function(y) ifelse(length(na.omit(y)) <= 3L, NA, suppressWarnings(misty::skewness(y, sample = sample, check = FALSE))), FUN.VALUE = double(1L)),
                           # Kurtosis
                           kurt = vapply(x, function(y) ifelse(length(na.omit(y)) <= 4L, NA, suppressWarnings(misty::kurtosis(y, sample = sample, check = FALSE))), FUN.VALUE = double(1L)),
                           # Median
                           med = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, median(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                           # Confidence interval for the median
                           low = vapply(x, .med.conf, alternative = alternative, conf.level = conf.level, side = "low", FUN.VALUE = double(1L)),
                           upp = vapply(x, .med.conf, alternative = alternative, conf.level = conf.level, side = "upp", FUN.VALUE = double(1L)),
                           row.names = NULL, check.names = FALSE)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bootstrapping ####

    } else {

      result.boot <- apply(matrix(seq_len(ncol(x)), nrow = 1L), 2L, function(y) x |>
                             (\(z) suppressWarnings(.ci.boot(data = x[, y], statistic = .boot.func.median, boot = boot, R = R, alternative = alternative, conf.level = conf.level, seed = seed)))() |>
                             (\(w) list(t = data.frame(variable = colnames(x)[y], med = w$t), result = data.frame(variable = colnames(x)[y], n = w$n, nNA = w$nNA, pNA = w$pNA, sd = w$sd, iqr = w$iqr, skew = w$skew, kurt = w$kurt, med = w$t0[1L], low = w$ci[1L], upp = w$ci[2L], row.names = NULL)))())

      boot.sample <- do.call("rbind", lapply(result.boot, function(y) y$t))
      result <- do.call("rbind", lapply(result.boot, function(y) y$result))

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, No Split ####

  } else if (isTRUE(!is.null(group) && is.null(split))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No Bootstrapping ####

    if (isTRUE(boot == "none")) {

      result <- lapply(split(x, f = group), function(y) misty::ci.median(y, data = NULL, group = NULL, split = NULL, alternative = alternative, conf.level = conf.level, sort.var = sort.var, check = FALSE, output = FALSE)$result) |>
        (\(y) data.frame(group = rep(names(y), each = ncol(x)), eval(parse(text = paste0("rbind(", paste0("y[[", seq_len(length(y)), "]]", collapse = ", "), ")")))) )()

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bootstrapping ####

    } else {

      result.boot <- lapply(split(x, f = group), function(y) misty::ci.median(y, data = NULL, group = NULL, split = NULL, sample = sample, seed = seed, boot = boot, R = R, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)) |>
        (\(z) list(boot.sample = data.frame(group = rep(names(z), each = R*unique(unlist(lapply(z, function(q) nrow(q$result))))), do.call("rbind", lapply(z, function(w) w$boot)), row.names = NULL), result = data.frame(group = rep(names(z), each = unique(unlist(lapply(z, function(q) nrow(q$result))))), do.call("rbind", lapply(z, function(w) w$result)), row.names = NULL)))()

      boot.sample <- result.boot$boot.sample
      result <- result.boot$result

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, Split ####

  } else if (isTRUE(is.null(group) && !is.null(split))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No Bootstrapping ####

    if (isTRUE(boot == "none")) {

      result <- lapply(split(data.frame(x), f = split), function(y) misty::ci.median(y, data = NULL, group = NULL, split = NULL, alternative = alternative, conf.level = conf.level, sort.var = sort.var, check = FALSE, output = FALSE)$result)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bootstrapping ####

    } else {

      result.boot <- lapply(split(x, f = split), function(y) misty::ci.median(y, data = NULL, group = NULL, split = NULL, sample = sample, seed = seed, boot = boot, R = R, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)) |>
        (\(z) list(boot.sample = data.frame(split = rep(names(z), each = R*unique(unlist(lapply(z, function(q) nrow(q$result))))), do.call("rbind", lapply(z, function(w) w$boot)), row.names = NULL), result = lapply(z, function(w) w$result)))()

      boot.sample <- result.boot$boot.sample
      result <- result.boot$result

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, Split ####

  } else if (isTRUE(!is.null(group) && !is.null(split))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No Bootstrapping ####

    if (isTRUE(boot == "none")) {

    result <- lapply(split(data.frame(x, group = group), f = split), function(y) misty::ci.median(y[, -grep("group", names(y))], group = y$group, split = NULL, alternative = alternative, conf.level = conf.level, sort.var = sort.var, check = FALSE, output = FALSE)$result)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bootstrapping ####

    } else {

      result.boot <- lapply(split(data.frame(x, group = group), f = split), function(y) misty::ci.median(y[, -grep("group", names(y))], group = y$group, split = NULL, sample = sample, seed = seed, boot = boot, R = R, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)) |>
        (\(z) list(boot.sample = data.frame(split = rep(names(z), each = R*unique(unlist(lapply(z, function(q) nrow(q$result))))), do.call("rbind", lapply(z, function(w) w$boot)), row.names = NULL), result = lapply(z, function(w) w$result)))()

      boot.sample <- result.boot$boot.sample
      result <- result.boot$result

    }

  }

  #_____________________________________________________________________________
  #
  # Plot -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Arithmetic Mean with Error Bars ####

  switch(plot, "ci" = {

    if (isTRUE(any(!is.na((if (isTRUE(!is.data.frame(result))) { do.call("rbind", result) } else { result })$low)))) {

      plot.object <- .plot.ci(result = result, stat = "med", group = group, split = split, point.size = point.size, point.shape = point.shape, errorbar.width = errorbar.width, dodge.width = dodge.width, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)

      #...................
      ### Print plot ####

      suppressWarnings(print(plot.object$p))

    } else {

      plot <- "none"

      warning("There are no confidence intervals for the arithmetic mean to plot.", call. = FALSE)

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Bootstrap Samples ####

  }, "boot" = {

    if (any(!is.na(boot.sample$m))) {

      plot.object <- .plot.boot(result = result, boot.sample = boot.sample, stat = "med", group = group, split = split, hist = hist, binwidth = binwidth, bins = bins, alpha = alpha, fill = fill, density = density, density.col = density.col, density.linewidth = density.linewidth, density.linetype = density.linetype, plot.point = plot.point, point.col = point.col, point.linewidth = point.linewidth, point.linetype = point.linetype, plot.ci = plot.ci, ci.col = ci.col, ci.linewidth = ci.linewidth, ci.linetype = ci.linetype, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales)

      #...................
      ### Print plot ####

      suppressMessages(suppressWarnings(print(plot.object$p)))

    } else {

      plot <- "none"

      warning("There are no bootstrap samples to plot.", call. = FALSE)

    }

  })

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "ci.median",
                 data = list(x = x, group = group, split = split),
                 args = list(boot = boot, R = R, seed = seed, sample = sample, alternative = alternative, conf.level = conf.level, sort.var = sort.var, na.omit = na.omit, digits = digits, as.na = as.na, plot = plot, point.size = point.size, point.shape = point.shape, errorbar.width = errorbar.width, dodge.width = dodge.width, hist = hist, binwidth = binwidth, bins = bins, alpha = alpha, fill = fill, density = density, density.col = density.col, density.linewidth = density.linewidth, density.linetype = density.linetype, plot.point = plot.point, point.col = point.col, point.linewidth = point.linewidth, point.linetype = point.linetype, plot.ci = plot.ci, ci.col = ci.col, ci.linewidth = ci.linewidth, ci.linetype = ci.linetype, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales, saveplot = saveplot, width = width, height = height, units = units, dpi = dpi, write = write, append = append, check = check, output = output),
                 boot = if (isTRUE(boot != "none")) { boot.sample } else { NULL },
                 plot = if (isTRUE(plot != "none")) { list(plot = plot.object$p, plotdata = plot.object$plotdat) } else { NULL },
                 result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) { .write.result(object = object, write = write, append = append) }

  #_____________________________________________________________________________
  #
  # Save ggplot ----------------------------------------------------------------

  if (isTRUE(plot != "none" && !is.null(saveplot))) { suppressWarnings(suppressMessages(ggplot2::ggsave(filename = saveplot, plot = plot.object$p, width = width, height = height, units = units, dpi = dpi))) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
