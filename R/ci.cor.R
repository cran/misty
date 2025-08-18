#' (Bootstrap) Confidence Intervals for Correlation Coefficients
#'
#' This function computes and plots (1) Fisher \eqn{z'} confidence intervals
#' for Pearson product-moment correlation coefficients (a) without non-normality
#' adjustment, (1b) adjusted via sample joint moments method or (1c) adjusted via
#' approximate distribution method (Bishara et al., 2018), (2) Spearman's rank-order
#' correlation coefficients with (2a) Fieller et al. (1957) standard error, (2b)
#' Bonett and Wright (2000) standard error, or (2c) rank-based inverse normal
#' transformation, (3) Kendall's Tau-b, and (4) Kendall-Stuart's Tau-c correlation
#' coefficients with Fieller et al. (1957) standard error, optionally by a grouping
#' and/or split variable. The function also supports five types of bootstrap
#' confidence intervals (e.g., bias-corrected (BC) percentile bootstrap or
#' bias-corrected and accelerated (BCa) bootstrap confidence intervals) and plots
#' the bootstrap samples with histograms and density curves. By default, the
#' function computes Pearson product-moment correlation coefficients adjusted via
#' approximate distribution method.
#'
#' @param data              a data frame with numeric variables, i.e., factors
#'                          and character variables are excluded from \code{data}
#'                          before conducting the analysis.
#' @param ...               an expression indicating the variable names in \code{data}
#'                          e.g., \code{ci.cor(x1, x2, data = dat)}. Note that the
#'                          operators \code{+}, \code{-}, \code{~}, \code{:},
#'                          \code{::}, and \code{!} can also be used to select
#'                          variables, see 'Details' in the \code{\link{df.subset}}
#'                          function.
#' @param method            a character string indicating which correlation
#'                          coefficient is to be computed, i.e., \code{"pearson"}
#'                          for Pearson product-moment correlation coefficient
#'                          (default), \code{"spearman"} for Spearman's rank-order
#'                          correlation coefficient, \code{"kendall-b"} for Kendall's
#'                          Tau-b correlation coefficient, \code{"kendall-c"} for
#'                          Kendall-Stuart's Tau-c correlation coefficient. Note
#'                          that confidence intervals are only computed given
#'                          at least 4 pairs of observations.
#' @param adjust            a character string specifying the non-normality
#'                          adjustment method, i.e., \code{"none"} for the Fisher
#'                          \eqn{z'} confidence interval for the Pearson
#'                          product-moment correlation coefficient without
#'                          non-normality adjustment, \code{"joint"} for the
#'                          confidence interval with non-normality adjustment via
#'                          sample joint moments, and \code{"approx"} (default)
#'                          for the confidence interval with non-normality adjustment
#'                          via approximate distribution by skewness and kurtosis.
#'                          Note that this argument only applies to the Pearson
#'                          product-moment correlation coefficient, i.e.,
#'                          \code{method = "pearson"}
#' @param se                a character string specifying the method for computing
#'                          the standard error of the correlation coefficient,
#'                          i.e., \code{"fisher"} for the Fisher \eqn{z'} confidence
#'                          interval, \code{"fieller"} (default) for the confidence
#'                          interval for Spearman's rank-order correlation
#'                          coefficient based on approximate standard error by
#'                          Fieller et al. (1957), \code{"bonett"} for the confidence
#'                          interval based on approximate standard error by Bonett
#'                          and Wright (2000), and \code{"rin"} for the confidence
#'                          interval for Spearman's rank-order correlation coefficient
#'                          based on rank-based inverse normal (RIN) transformation.
#'                          Note that this argument only applies to Spearman's
#'                          rank-order correlation coefficient, i.e.,
#'                          \code{method = "spearman"}.
#' @param sample            logical: if \code{TRUE} (default), the univariate
#'                          sample skewness and kurtosis is used when applying
#'                          the approximate distribution method and reported in
#'                          the result table, while the population skewness and
#'                          kurtosis is used when \code{sample = FALSE}.
#' @param seed              a numeric value specifying the seed of the pseudo-random
#'                          number generator when generating a random set of
#'                          starting parameter value when the parameters led to
#'                          a sum of squares greater than the maximum tolerance
#'                          after optimization when applying the approximate
#'                          distribution method (\code{adjust = approx}) when
#'                          computing the confidence interval for the Pearson
#'                          product-moment correlation coefficient, or the seeds
#'                          of the pseudo-random numbers used when conducting
#'                          bootstrapping.
#' @param maxtol            a numeric value indicating the tolerance for total
#'                          squared error when applying the approximate distribution
#'                          method (\code{adjust = approx}).
#' @param nudge             a numeric value indicating the nudge proportion of
#'                          their original values by which sample skewness, kurtosis,
#'                          and r are nudged towards 0 when applying the approximate
#'                          distribution method (\code{adjust = approx}).
#'                          are only computed given at least 10 pairs of observations.
#' @param boot              a character string specifying the type of bootstrap
#'                          confidence intervals (CI), i.e., \code{"none"} (default)
#'                          for not conducting bootstrapping, \code{"norm"} for
#'                          the bias-corrected normal approximation bootstrap CI,
#'                          \code{"basic"} for the basic bootstrap CI, \code{"perc"},
#'                          for the percentile bootstrap CI \code{"bc"} (default)
#'                          for the bias-corrected (BC) percentile bootstrap CI
#'                          (without acceleration), and \code{"bca"} for the
#'                          bias-corrected and accelerated (BCa) bootstrap CI.
#' @param R                 a numeric value indicating the number of bootstrap
#'                          replicates (default is 1000).
#' @param fisher            logical: if \code{TRUE} (default), Fisher \eqn{z}
#'                          transformation is applied before computing the
#'                          confidence intervals to reverse-transformed the limits
#'                          of the interval using the inverse of the Fisher
#'                          \eqn{z} transformation. Note that this argument applies
#'                          only  when \code{boot} is \code{"norm"} or \code{"basic"}.
#' @param alternative       a character string specifying the alternative hypothesis,
#'                          must be one of \code{"two.sided"} (default),
#'                          \code{"greater"} or \code{"less"}.
#' @param conf.level        a numeric value between 0 and 1 indicating the confidence
#'                          level of the interval.
#' @param group             either a character string indicating the variable name
#'                          of the grouping variable in \code{data}, or a vector
#'                          representing the grouping variable. The grouping variable
#'                          is excluded from the data frame specified in \code{data}.
#' @param split             either a character string indicating the variable name
#'                          of the split variable in \code{data}, or a vector
#'                          representing the split variable. The split variable
#'                          is excluded from the data frame specified in \code{data}.
#' @param na.omit           logical: if \code{TRUE}, incomplete cases are removed
#'                          before conducting the analysis (i.e., listwise deletion);
#'                          if \code{FALSE} (default), pairwise deletion is used.
#' @param digits            an integer value indicating the number of decimal
#'                          places to be used.
#' @param as.na             a numeric vector indicating user-defined missing values,
#'                          i.e. these values are converted to \code{NA} before
#'                          conducting the analysis.
#' @param plot              a character string indicating the type of the plot
#'                          to display, i.e., \code{"none"} (default) for not
#'                          displaying any plots, \code{"ci"} for displaying
#'                          confidence intervals for the correlation coefficient,
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
#'                          symbols of points. when plotting confidence intervals
#'                          (\code{plot = "ci"}).
#' @param errorbar.width    a numeric value indicating the \code{width} argument
#'                          in the \code{geom_errorbar} function for controlling
#'                          the width of the whiskers in the \code{geom_errorbar}
#'                          function when plotting confidence intervals
#'                          (\code{plot = "ci"}).
#' @param dodge.width       a numeric value indicating the \code{width} argument
#'                          controlling the width of the \code{geom} elements to
#'                          be dodged when specifying a grouping variable using
#'                          the argument \code{group} when plotting confidence
#'                          intervals (\code{plot = "ci"}).
#' @param hist              logical: if \code{TRUE} (default), histograms are
#'                          drawn when plotting bootstrap samples (\code{plot = "boot"}).
#' @param binwidth          a numeric value or a function for specifying the
#'                          \code{binwidth} argument in the \code{geom_histogram}
#'                          function for controlling the width of the bins when
#'                          plotting bootstrap samples (\code{plot = "boot"}).
#' @param bins              a numeric value for specifying the \code{bins} argument
#'                          in the \code{geom_histogram} function for controlling
#'                          the number of bins when plotting bootstrap samples
#'                          (\code{plot = "boot"}).
#' @param hist.alpha        a numeric value between 0 and 1 for specifying the
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
#'                          drawn when plotting bootstrap samples
#'                          (\code{plot = "boot"}).
#' @param density.col       a character string specifying the \code{color} argument
#'                          in the \code{geom_density} function controlling the
#'                          color of the density curves when plotting bootstrap samples
#'                          (\code{plot = "boot"}). Note that this argument applied
#'                          only when no grouping variable was specified
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
#' @param point             logical: if \code{TRUE} (default), vertical lines
#'                          representing the point estimate of the correlation
#'                          coefficients are drawn when plotting bootstrap samples
#'                          (\code{plot = "boot"}).
#' @param point.col         a character string specifying the \code{color} argument
#'                          in the \code{geom_vline} function for controlling the
#'                          color of the vertical line displaying the correlation
#'                          coefficient when plotting bootstrap samples
#'                          (\code{plot = "boot"}). Note that this argument applied
#'                          only when no grouping variable was specified
#'                          \code{group = NULL}.
#' @param point.linewidth   a numeric value specifying the \code{linewdith} argument
#'                          in the \code{geom_vline} function for controlling the
#'                          line width of the vertical line displaying the
#'                          correlation coefficient when plotting bootstrap samples
#'                          (\code{plot = "boot"}).
#' @param point.linetype    a numeric value or character string specifying the
#'                          \code{linetype} argument in the \code{geom_vline}
#'                          function controlling the line type of the vertical
#'                          line displaying the correlation  coefficient when
#'                          plotting bootstrap samples (\code{plot = "boot"}).
#' @param ci                logical: if \code{TRUE} (default), vertical lines
#'                          representing the bootstrap confidence intervals of
#'                          the correlation coefficient are drawn when plotting
#'                          bootstrap samples (\code{plot = "boot"}).
#' @param ci.col            character string specifying the \code{color} argument
#'                          in the \code{geom_vline} function for controlling the
#'                          color of the vertical line displaying bootstrap
#'                          confidence intervals when plotting bootstrap samples
#'                          (\code{plot = "boot"}). Note that this argument applied
#'                          only when no grouping variable was specified
#'                          \code{group = NULL}.
#' @param c.linewidth       a numeric value specifying the \code{linewdith} argument
#'                          in the \code{geom_vline} function for controlling the
#'                          line width of the vertical line displaying bootstrap
#'                          confidence intervals when plotting bootstrap samples
#'                          (\code{plot = "boot"}).
#' @param ci.linetype       a numeric value or character string specifying the
#'                          \code{linetype} argument in the \code{geom_vline}
#'                          function controlling the line type of the vertical
#'                          line displaying bootstrap confidence intervals when
#'                          plotting bootstrap samples (\code{plot = "boot"}).
#' @param line              logical: if \code{TRUE} (default), a horizontal line
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
#'                          when \code{plot = "ci"} and \code{xlab = "Correlation Coefficient"}
#'                          when \code{plot = "boot"}.
#' @param ylab              a character string indicating the \code{name} argument
#'                          in the \code{scale_y_continuous} function for labeling
#'                          the y-axis. The default setting is \code{ylab = "Correlation Coefficient"}
#'                          when \code{plot = "ci"} and \code{ylab = "Probability Density, f(x)"}
#'                          when \code{plot = "boot"}.
#' @param xlim              a numeric vector with two elements indicating the
#'                          \code{limits} argument in the \code{scale_x_continuous}
#'                          function for controlling the scale range of the x-axis.
#'                          The default setting is \code{xlim = NULL}
#'                          when \code{plot = "ci"} and \code{xlim = c(-1, 1)}
#'                          when \code{plot = "boot"}.
#' @param ylim              a numeric vector with two elements indicating the
#'                          \code{limits} argument in the \code{scale_y_continuous}
#'                          function for controlling the scale range of the y-axis.
#'                          The default setting is \code{ylim = c(-1, 1)} when
#'                          \code{plot = "ci"} and \code{xlim = NULL} when
#'                          \code{plot = "boot"}.
#' @param xbreaks           a numeric vector indicating the \code{breaks} argument
#'                          in the \code{scale_x_continuous} function for controlling
#'                          the x-axis breaks.
#' @param ybreaks           a numeric vector indicating the \code{breaks} argument
#'                          in the \code{scale_y_continuous} function for controlling
#'                          the y-axis breaks.
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
#'                          \code{"free_x"}, \code{"free_y"} (default), or
#'                          \code{"free"} when specifying a split variable using
#'                          the argument \code{split}.
#' @param filename          a character string indicating the \code{filename}
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
#' @details
#' \describe{
#' \item{\strong{Pearson Product-Moment Correlation Coefficient}}{The Fisher
#' \eqn{z'} confidence interval method for the Pearson product-moment correlation
#' coefficient is based on the assumption that \eqn{X} and \eqn{Y} have a bivariate
#' normal distribution in the population. Non-normality resulting from either
#' high kurtosis or high absolute skewness can distort the Fisher \eqn{z'}
#' confidence interval that produces a coverage rate that does not equal the one
#' intended. The distortion is largest when population correlation is large and
#' both variables \eqn{X} and \eqn{Y} were non-normal (Bishara et al., 2017).
#' Note that increasing sample size improves coverage only when the population
#' correlation is zero, while increasing the sample size worsens coverage with a
#' non-zero population correlation (Bishara & Hittner, 2017). The \code{ci.cor}
#' function computes the Fisher \eqn{z'} confidence interval without non-normality
#' adjustment (\code{adjust = "none"}), with non-normality adjustment via sample
#' joint moments (\code{adjust = "joint"}), or with non-normality adjustment via
#' approximate distribution (\code{adjust = "approx"}):
#'
#' \itemize{
#'      \item{\emph{Fisher \eqn{z'} confidence interval method}} uses the
#'      \eqn{r}-to\eqn{z'} transformation for the correlation coefficient
#'      \eqn{r}:
#'
#'      \deqn{z' = 0.5\cdot \ln\left(\frac{1 + r}{1 - r}\right)}
#'
#'      The sampling distribution of \eqn{z} is approximately normal with a
#'      standard error of approximately
#'
#'      \deqn{\sigma_z' = \sqrt{\frac{1}{(n - 3)}}}
#'
#'      The two-sided 95\% confidence interval is defined as
#'
#'      \deqn{z' \pm 1.96\cdot\sigma_{z'}}
#'
#'      These confidence interval bounds are transformed back to the scale of
#'      \eqn{r}:
#'
#'      \deqn{r = \frac{exp(2z') - 1}{exp(2z') + 1}}
#'
#'      The resulting confidence interval of the correlation coefficient is an
#'      approximation and is only accurate when \eqn{X} and \eqn{Y} have a
#'      bivariate normal distribution in the population or when the population
#'      correlation is zero.
#'
#'      \item{The \emph{Joint Moments Method}} multiplies the asymptotic variance of
#'      \eqn{z'} by \eqn{\tau_f^2} (Hawkins, 1989):
#'
#'      \deqn{\tau_f^2 = \frac{(\mu_{40} + 2\mu_{22} + \mu_{04})\rho^2 - 4(\mu_{31} + \mu_{13})\rho + 4\mu_{22})}{4(1 - \rho^2)^2}}
#'
#'      where \eqn{\mu_{jk}} represents a population joint moment defined as
#'
#'      \deqn{\mu_{jk} = E[X^jY^k]}
#'
#'      where \eqn{X} and \eqn{Y} are assumed to be standardized (\eqn{\mu_{10} = \mu_{01} = 0},
#'      \eqn{\mu_{20} = \mu_{02} = 1}). The standard error of \eqn{z}' can then be
#'      approximated as \eqn{\tilde{\sigma}_{z'}}:
#'
#'      \deqn{\tilde{\sigma}_{z'} = \tau_f\sqrt{\frac{1}{n - 3}}}
#'
#'      The corresponding sample moments, \eqn{m_{jk}} can be used to estimate
#'      \eqn{\tau_f^2}:
#'
#'      \deqn{\hat{\mu}_{jk} = m_{jk} = \frac{1}{n}\sum_{i=1}^{n}(x_i^jy_i^k)}
#'
#'      However, the higher-order sample joint moments may be unstable estimators
#'      of their population counterparts unless the sample size is extremely
#'      large. Thus, this estimate of \eqn{\tau_f^2} may be inaccurate, leading
#'      to inaccurate confidence intervals.
#'
#'      \item{The \emph{Approximate Distribution Method}} estimates an approximate
#'      distribution that the sample appears to be drawn from to analytically
#'      solve for \eqn{\tau_f^2} based on that distribution's parameters. The
#'      \code{ci.cor} function uses a third-order polynomial family allowing
#'      estimation of distribution parameters using marginal skewness and kurtosis
#'      that are estimated using the marginal sample skewness and kurtosis
#'      statistics (Bishara et al., 2018).
#' }
#' Bishara et al. (2018) conducted two Monte Carlo simulations that showed that
#' the approximate distribution method was effective in dealing with violations
#' of the bivariate normality assumption for a wide range of sample sizes, while
#' the joint moments method was effective mainly when the sample size was
#' extremely large, in the thousands. However, the third-order polynomial family
#' used for the approximate distribution method cannot deal with absolute skewness
#' above 4.4 or kurtosis above 43.4. Note that the approximate distribution method
#' is accurate even when the bivariate normality assumption is satisfied, while
#' the sample joint moments method sometimes fails to achieve the intended coverage
#' even when the bivariate normality was satisfied.}
#'
#' \item{\strong{Spearman's Rank-Order Correlation Coefficient}}{The confidence
#' interval for Spearman's rank-order correlation coefficient is based on the
#' Fisher's \eqn{z} method (\code{se = "fisher"}), Fieller et al. (1957)
#' approximate standard error (\code{se = "fieller"}, default), Bonett and Wright
#' (2000) approximate standard error (\code{se = "bonett"}) or rank-based inverse
#' normal (RIN) transformation (\code{se = "rin"}) :
#'
#' \itemize{
#'      \item{\emph{Fisher's \eqn{z} Standard Error}}
#'      \deqn{\sqrt{\frac{1}{(n - 3)}}}
#'
#'      \item{\emph{Fieller et al. (1957) Approximate Standard Error}}
#'
#'      \deqn{\sqrt{\frac{1.06}{(n - 3)}}}
#'
#'      Note that this approximation for the standard error is recommended for
#'      \eqn{n > 10} and \eqn{|rs| < 0.8}.
#'
#'      \item{\emph{Bonett and Wright (2000) Approximate Standard Error}}
#'
#'      \deqn{\sqrt{\frac{1 + \frac{\hat{\theta}^2}{2}}{(n - 3)}}}
#'
#'      where \eqn{\hat{\theta}} is the point estimate of the Spearman's rank-order
#'      correlation coefficient. Note that this approximation for the standard
#'      error is recommended for \eqn{|\tau| \le 0.9}.
#'
#'      \item{\emph{RIN Transformation}} involves three steps. First, the variable
#'      is converted to ranks. Second, the ranks are converted to a 0-to-1 scale
#'      using a linear function. Third, this distribution is transformed via the
#'      inverse of the normal cumulative distribution function (i.e., via probit
#'      transformation). The result is an approximately normal distribution
#'      regardless of the original shape of the data, so long as ties are
#'      infrequent and \eqn{n} is not too small.
#' }
#' }
#' \item{\strong{Kendall's Tau-b and Tau-c Correlation Coefficient}}{The confidence
#' interval for Kendall's Tau-b and Tau-c correlation coefficient is based on the
#' approximate standard error by Fieller et al. (1957):
#'
#' \deqn{\sigma_z' = \sqrt{\frac{0.437}{(n - 4)}}}
#'
#' Note that this approximation for the standard error is recommended for
#' \eqn{n > 10} and \eqn{|\tau| < 0.8}.
#' }
#' \item{\strong{Bootstrap Confidence Intervals}}{The \code{ci.cor} function supports
#' bootstrap confidence intervals (CI) for the correlation coefficient by changing
#' the default setting \code{boot = "none"} to request one of five different types
#' of bootstrap CI (see Efron & Tibshirani, 1993; Davidson & Hinkley, 1997):
#'
#' \itemize{
#'
#' \item{\code{"norm"}:} The bias-corrected normal approximation bootstrap CI
#' relies on the normal distribution based on the standard deviation of the
#' bootstrap samples \eqn{\hat{\mathit{SE}}^*}. The function corrects for the
#' bootstrap bias, i.e., difference between the bootstrap estimate \eqn{\hat{\theta}^*}
#' and the sample statistic \eqn{\hat{\theta}} centering the interval at \eqn{2\hat{\theta} - \hat{\theta}^*}.
#' The BC normal CI of intended coverage of \eqn{1 - 2(\alpha/2)} is given by
#'
#' \deqn{Normal: (\hat{\theta}_{low}, \hat{\theta}_{upp} = \hat{\theta} - (\hat{\theta}^* - \hat{\theta}) + z^{\alpha/2} \cdot \hat{\mathit{SE}}^*, \hat{\theta} - (\hat{\theta}^* - \hat{\theta}) + z^{1 - \alpha/2} \cdot \hat{\mathit{SE}}^*}
#'
#' where \eqn{z^{\alpha/2}} and \eqn{z^{1 - \alpha/2}} denotes the \eqn{\alpha} and
#' the \eqn{1 - \alpha} quantile from the standard normal distribution.
#'
#' \item{\code{"basic"}:} The basic bootstrap (aka reverse bootstrap percentile)
#' CI is based on the distribution of \eqn{\hat{\delta} = \hat{\theta} - \theta}
#' which is approximated with the bootstrap distribution of
#' \eqn{\hat{\delta}^* = \hat{\theta}^* - \hat{\theta}}.
#'
#' \deqn{Basic: (\hat{\theta}_{low}, \hat{\theta}_{upp} = \hat{\theta} - \hat{\delta}^{*1 - (\alpha/2)}, \hat{\theta} - \hat{\delta}^{*\alpha/2} = 2\hat{\theta} - \hat{\theta}^{*(1 - \alpha/2)} , 2\hat{\theta} - \hat{\theta}^{*(\alpha/2)})}
#'
#' \item{\code{"perc"}:} The percentile bootstrap CI is computed by ordering the
#' bootstrap estimates \eqn{\hat{\theta}^*_1, \ldots, \hat{\theta}^*_B} to determine
#' the \eqn{(100(\alpha)/2)}th and \eqn{(100(1 - \alpha)/2)}th empirical percentile
#' with intended coverage of \eqn{1 - 2(\alpha/2)}:
#'
#' \deqn{Percentile: (\hat{\theta}_{low}, \hat{\theta}_{upp} = \hat{\theta}^{*(1 - \alpha/2)}, \hat{\theta}^{*(\alpha/2)})}
#'
#' \item{\code{"bc"} (default):} The bias-corrected (BC) percentile bootstrap CI corrects
#' the percentile bootstrap CI for median bias of \eqn{\hat{\theta^*}}, i.e., the
#' discrepancy between the median of \eqn{\hat{\theta}^*} and \eqn{\hat{\theta}}
#' in normal units. The bias correction \eqn{\hat{z}_0} is obtained from the
#' proportion of bootstrap replications less than the sample estimate \eqn{\hat{\theta}}:
#'
#' \deqn{\hat{z}_0 = \Phi^{-1}\left(\frac{\#{\hat{\theta}^*_b < \hat{\theta}}}{B}\right)}
#'
#' where \eqn{\Phi^{-1}(.)} represents the inverse function of the standard normal
#' cumulative distribution function and \eqn{B} is the number of bootstrap
#' replications. The BC percentile CI of intended coverage of \eqn{1 - 2(\alpha/2)}
#' is given by
#'
#' \deqn{BC: (\hat{\theta}_{low}, \hat{\theta}_{upp} = \hat{\theta}^{*(\alpha_1)}, \hat{\theta}^{*(\alpha_2)})}
#'
#' where
#'
#' \deqn{\alpha_1 = \Phi(2\hat{z}_0 + z^{\alpha/2})}
#'
#' \deqn{\alpha_2 = \Phi(2\hat{z}_0 + z^{1 - \alpha/2})}
#'
#' where \eqn{\Phi(.)} represents the standard normal cumulative distribution function
#' and \eqn{z^{\alpha/2}} is the \eqn{100(\alpha/2)} percentile
#' of a standard normal distribution.
#'
#' \item{\code{"bca"}:} The bias-corrected and accelerated (BCa) bootstrap CI
#' corrects the percentile bootstrap CI for median bias \eqn{\hat{z}_0} and
#' for  acceleration or skewness \eqn{\hat{a}}, i.e., the rate of change of the
#' standard error of \eqn{\hat{\theta}} with respect to the true parameter value
#' \eqn{\theta} on a normalized scale. The standard normal approximation
#' \eqn{\hat{\theta} \sim N(\theta, \mathit{SE}^2)} assumes that the standard error of
#' \eqn{\hat{\theta}} is the same for all \eqn{\theta}. The acceleration constant
#' \eqn{\hat{a}} corrects for this unrealistic assumption and can be computed by
#' using jackknife resampling:
#'
#' \deqn{\hat{a} = \frac{\sum_{i=1}^{n}(\hat{\theta}_{(.)} - \hat{\theta}_{(i)})^3}{6\{\sum_{i=1}^{n}(\theta_{(.)} - \hat{\theta}_{(i)})^2\}^{3/2}}}
#'
#' where \eqn{\hat{\theta}_{(i)}} is the sample estimate with the \eqn{i}th
#' observation deleted and \eqn{\hat{\theta}_{(.)} = \sum_{i=1}^{n}\frac{\hat{\theta}_{(i)}}{n}}.
#' Note that the function uses infinitesimal jackknife instead of regular
#' leave-one-out jackknife that down-weights each observation by an infinitesimal
#' amount of \eqn{\frac{0.001}{n}} instead of removing observations. The BCa
#' percentile CI of intended coverage of \eqn{1 - 2(\alpha/2)}
#' is given by
#'
#' \deqn{BCa: (\hat{\theta}_{low}, \hat{\theta}_{upp} = \hat{\theta}^{*(\alpha_1)}, \hat{\theta}^{*(\alpha_2)})}
#'
#' where
#'
#' \deqn{\alpha_1 = \Phi\left(\hat{z}_0 + \frac{\hat{z}_0 + z^{\alpha/2}}{1 - \hat{a}(\hat{z}_0 + z^{\alpha/2})}\right)}
#'
#' \deqn{\alpha_2 = \Phi\left(\hat{z}_0 + \frac{\hat{z}_0 + z^{1 - \alpha/2}}{1 - \hat{a}(\hat{z}_0 + z^{1 - \alpha/2})}\right)}
#' }
#' Note that Fisher transformation is applied before computing the confidence
#' intervals to reverse-transform the limits of the interval using the inverse
#' of the Fisher \eqn{z} transformation (\code{fisher = TRUE}) when specifying
#' \code{"norm"} or \code{"basic"} for the argument \code{boot}. In addition,
#' interpolation on the normal quantile scale is applied for \code{"basic"},
#' \code{"perc"}, \code{"bc"}, and \code{"bca"} when a non-integer order
#' statistic is required (see equation 5.8 in Davison & Hinkley, 1997).
#' }
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{cor.matrix}}, \code{\link{ci.mean}}, \code{\link{ci.mean.diff}},
#' \code{\link{ci.prop}}, \code{\link{ci.var}}, \code{\link{ci.sd}}
#'
#' @references
#' Baguley, T. (2024). \emph{CI for Spearman's rho}. seriousstats.
#' https://rpubs.com/seriousstats/616206
#'
#' Bonett, D. G., & Wright, T. A. (2000). Sample size requirements for estimating
#' Pearson, Kendall and Spearman correlations. \emph{Psychometrika, 65}, 23-28.
#' https://doi.org/10.1007/BF02294183
#'
#' Bishara, A. J., & Hittner, J. B. (2012). Testing the significance of a correlation
#' with nonnormal data: Comparison of Pearson, Spearman, transformation, and resampling
#' approaches. \emph{Psychological Methods, 17}(3), 399–417.
#' https://doi.org/10.1037/a0028087
#'
#' Bishara, A. J., & Hittner, J.B. (2017). Confidence intervals for correlations
#' when data are not normal. \emph{Behavior Research Methods, 49}, 294-309.
#' https://doi.org/10.3758/s13428-016-0702-8
#'
#' Bishara, A. J., Li, J., & Nash, T. (2018). Asymptotic confidence intervals for
#' the Pearson correlation via skewness and kurtosis. \emph{British Journal of
#' Mathematical and Statistical Psychology, 71}(1), 167–185.
#' https://doi.org/10.1111/bmsp.12113
#'
#' Brown, M. B., & Benedetti, J. K. (1977). Sampling behavior of tests for correlation
#' in two-way contingency tables. \emph{Journal of the American Statistical Association, 72}
#' (358), 309-315. https://doi.org/10.1080/01621459.1977.10480995
#'
#' Canty, A., & Ripley, B. (2024). \emph{boot: Bootstrap R (S-Plus) Functions}.
#' R package version 1.3-31.
#'
#' Davison, A. C., & Hinkley, D. V. (1997). \emph{Bootstrap methods and their application}.
#' Cambridge University Press.
#'
#' Efron, B., & Tibshirani, R. (1993). \emph{An introduction to the bootstrap}.
#' Chapman & Hall.
#'
#' Fieller, E. C., Hartley, H. O., & Pearson, E. S. (1957). Tests for rank
#' correlation coefficients: I. \emph{Biometrika, 44}, 470-481.
#' https://doi.org/10.2307/2332878
#'
#' Fisher, R. A. (1921). On the “Probable Error” of a Coefficient of Correlation
#' Deduced from a Small Sample. \emph{Metron}, 1, 3-32.
#'
#' Hawkins, D. L. (1989). Using U statistics to derive the asymptotic distribution
#' of Fisher’s Z statistic. \emph{American Statistician, 43}, 235–237.
#' https://doi.org/10.2307/2685369
#'
#' Hollander, M., Wolfe, D. A., & Chicken, E. (2015). \emph{Nonparametric statistical
#' methods}. Wiley.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{list with the input specified in \code{data}, \code{group}, and \code{split}}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{boot}}{data frame with bootstrap replicates of the correlation coefficient when bootstrapping was requested}
#' \item{\code{plot}}{ggplot2 object for plotting the results}
#' \item{\code{result}}{result table}
#'
#' @note
#' This function is based on a modified copy of the functions provided in the
#' supporting information in Bishara et al. (2018) for the sample joint moments
#' method and approximate  distribution method, functions provided in the
#' supplementary materials in Bishara and Hittner (2017) for Fieller et al. (1957)
#' and Bonett and Wright (2000) correction, and a function provided by Thom Baguley
#' (2024) for the rank-based inverse normal (RIN) transformation. Bootstrap confidence
#' intervals are computed using the R package \code{boot} by Angelo Canty and
#' Brain Ripley (2024).
#'
#' @export
#'
#' @examples
#' #----------------------------------------------------------------------------
#' # Pearson product-moment correlation coefficient
#'
#' # Example 1a: Approximate distribution method
#' ci.cor(mtcars, mpg, drat, qsec)
#'
#' # Alternative specification without using the '...' argument
#' ci.cor(mtcars[, c("mpg", "drat", "qsec")])
#'
#' # Example 1b: Joint moments method
#' ci.cor(mtcars, mpg, drat, qsec, adjust = "joint")
#'
#' #----------------------------------------------------------------------------
#' # Spearman's rank-order correlation coefficient
#'
#' # Example 2a: Fieller et al. (1957) approximate standard error
#' ci.cor(mtcars, mpg, drat, qsec, method = "spearman")
#'
#' # Example 2b: Bonett and Wright (2000) approximate standard error
#' ci.cor(mtcars, mpg, drat, qsec, method = "spearman", se = "bonett")
#'
#' # Example 2c: Rank-based inverse normal (RIN) transformation
#' ci.cor(mtcars, mpg, drat, qsec, method = "spearman", se = "rin")
#'
#' #----------------------------------------------------------------------------
#' # Kendall's Tau
#'
#' # Example 3a:  Kendall's Tau-b
#' ci.cor(mtcars, mpg, drat, qsec, method = "kendall-b")
#'
#' # Example 3b:  Kendall's Tau-c
#' ci.cor(mtcars, mpg, drat, qsec, method = "kendall-c")
#'
#' \dontrun{
#' #----------------------------------------------------------------------------
#' # Bootstrap Confidence Interval (CI)
#'
#' # Example 4a: Bias-corrected (BC) percentile bootstrap CI
#' ci.cor(mtcars, mpg, drat, qsec, boot = "bc")
#'
#' # Example 4b: Bias-corrected and accelerated (BCa) bootstrap CI,
#' # 5000 bootstrap replications, set seed of the pseudo-random number generator
#' ci.cor(mtcars, mpg, drat, qsec, boot = "bca", R = 5000, seed = 123)
#'
#' #----------------------------------------------------------------------------
#' # Grouping and Split Variable
#'
#' # Example 5a: Grouping variable
#' ci.cor(mtcars, mpg, drat, qsec, group = "vs")
#'
#' # Alternative specification without using the argument '...'
#' ci.cor(mtcars[, c("mpg", "drat", "qsec")], group = mtcars$vs)
#'
#' # Example 5b: Split variable
#' ci.cor(mtcars, mpg, drat, qsec, split = "am")
#'
#' # Alternative specification without using the argument '...'
#' ci.cor(mtcars[, c("mpg", "drat", "qsec")], split = mtcars$am)
#'
#' # Example 5c: Grouping and split variable
#' ci.cor(mtcars, mpg, drat, qsec, group = "vs", split = "am")
#'
#' # Alternative specification without using the argument '...'
#' ci.cor(mtcars[, c("mpg", "drat", "qsec")], group = mtcars$vs, split = mtcars$am)
#'
#' #----------------------------------------------------------------------------
#' # Write Output
#'
#' # Example 6a: Text file
#' ci.cor(mtcars, mpg, drat, qsec, write = "CI_Cor_Text.txt")
#'
#' # Example 6b: Excel file
#' ci.cor(mtcars, mpg, drat, qsec, write = "CI_Cor_Excel.xlsx")
#'
#' #----------------------------------------------------------------------------
#' # Plot Confidence Intervals
#'
#' # Example 7a: Pearson product-moment correlation coefficient
#' ci.cor(mtcars, mpg, drat, qsec, plot = "ci")
#'
#' # Example 7b: Grouping variable
#' ci.cor(mtcars, mpg, drat, qsec, group = "vs", plot = "ci")
#'
#' # Example 7c: Split variable
#' ci.cor(mtcars, mpg, drat, qsec, split = "am", plot = "ci")
#'
#' # Example 7d: Save plot as PDF file
#' ci.cor(mtcars, mpg, drat, qsec, plot = "ci", filename = "CI_Cor.pdf",
#'        width = 8, height = 6)
#'
#' # Example 7e: Save plot as PNG file
#' ci.cor(mtcars, mpg, drat, qsec, plot = "ci", filename = "CI_Cor.png",
#'        width = 8, height = 6)
#'
#' #----------------------------------------------------------------------------
#' # Plot Bootstrap Samples
#'
#' # Example 8a: Pearson product-moment correlation coefficient
#' ci.cor(mtcars, mpg, drat, qsec, boot = "bc", plot = "boot")
#'
#' # Example 8b: Grouping variable
#' ci.cor(mtcars, mpg, drat, qsec, group = "vs", boot = "bc", plot = "boot")
#'
#' # Example 8c: Split variable
#' ci.cor(mtcars, mpg, drat, qsec, split = "am", boot = "bc", plot = "boot")
#'
#' # Example 8d: Save plot as PDF file
#' ci.cor(mpg, drat, qsec, data = mtcars, boot = "bc", plot = "boot",
#'        filename = "CI_Cor_Boot.pdf", width = 14, height = 9)
#'
#' # Example 8e: Save plot as PNG file
#' ci.cor(mtcars, mpg, drat, qsec, boot = "bc", plot = "boot",
#'        filename = "CI_Cor_Boot.png", width = 14, height = 9)
#' }
ci.cor <- function(data, ...,
                   method = c("pearson", "spearman", "kendall-b", "kendall-c"),
                   adjust = c("none", "joint", "approx"),
                   se = c("fisher", "fieller", "bonett", "rin"),
                   sample = TRUE, seed = NULL, maxtol = 1e-05, nudge = 0.001,
                   boot = c("none", "norm", "basic", "perc", "bc", "bca"), R = 1000,
                   fisher = TRUE, alternative = c("two.sided", "less", "greater"),
                   conf.level = 0.95, group = NULL, split = NULL, na.omit = FALSE, digits = 2,
                   as.na = NULL, plot = c("none", "ci", "boot"), point.size = 2.5,
                   point.shape = 19, errorbar.width = 0.3, dodge.width = 0.5, hist = TRUE,
                   binwidth = NULL, bins = NULL, hist.alpha = 0.4, fill = "gray85", density = TRUE,
                   density.col = "#0072B2", density.linewidth = 0.5, density.linetype = "solid",
                   point = TRUE, point.col = "#CC79A7", point.linewidth = 0.6,
                   point.linetype = "solid", ci = TRUE, ci.col = "black",
                   ci.linewidth = 0.6, ci.linetype = "dashed", line = TRUE, intercept = 0,
                   linetype = "solid", line.col = "gray65", xlab = NULL, ylab = NULL,
                   xlim = NULL, ylim = NULL, xbreaks = ggplot2::waiver(), ybreaks = ggplot2::waiver(),
                   axis.title.size = 11, axis.text.size = 10, strip.text.size = 11, title = NULL,
                   subtitle = NULL, group.col = NULL, plot.margin = NA,  legend.title = "",
                   legend.position = c("right", "top", "left", "bottom", "none"),
                   legend.box.margin = c(-10, 0, 0, 0), facet.ncol = NULL, facet.nrow = NULL,
                   facet.scales = "free_y", filename = NULL, width = NA, height = NA,
                   units = c("in", "cm", "mm", "px"), dpi = 600, write = NULL,
                   append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a data frame with numeric variables for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument '...' ####

  if (isTRUE(!missing(...))) {

    # Extract data and convert tibble into data frame or vector
    x <- as.data.frame(data[, .var.names(data = data, ..., group = group, split = split)])

    # Grouping variable
    if (isTRUE(!is.null(group))) { group <- data[, group] }

    # Splitting variable
    if (isTRUE(!is.null(split))) { split <- data[, split] }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument '...' ####

  } else {

    # Convert 'data' as tibble into data frame
    x <- as.data.frame(data)

    # Data and cluster
    var.group <- .var.group(data = x, group = group, split = split)

    # Data
    if (isTRUE(!is.null(var.group$data)))  { x <- var.group$data }

    # Grouping variable
    if (isTRUE(!is.null(var.group$group))) { group <- var.group$group }

    # Split variable
    if (isTRUE(!is.null(var.group$split))) { split <- var.group$split }

  }

  # Convert 'group' and 'split' as tibble into a vector
  if (!is.null(group) && isTRUE("tbl" %in% substr(class(group), 1L, 3L))) { group <- unname(unlist(group)) }
  if (!is.null(split) && isTRUE("tbl" %in% substr(class(split), 1L, 3L))) { split <- unname(unlist(split)) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping and Split Variable ####

  # Grouping variable
  if (!is.null(group)) {

    x <- which(sapply(names(x), function(y) identical(group, x[, y]))) |> (\(z) if (isTRUE(length(z) != 0L)) { return(x[, -z]) } else { x })()

    if (isTRUE(ncol(x) == 0L)) { stop("After excluding the grouping variable from the data frame, there are not enough variables left for computing a correlation coefficient.") }

  }

  # Split variable
  if (!is.null(split)) {

    x <- which(sapply(names(x), function(y) identical(split, x[, y]))) |> (\(z) if (isTRUE(length(z) != 0L)) { return(x[, -z]) } else { x })()

    if (isTRUE(ncol(x) == 0L)) { stop("After excluding the split variable from the data frame, there are not enough variables left for computing a correlation coefficient.") }

  }

  # Grouping and split variable are identical
  if (isTRUE(!is.null(group) && !is.null(split) && identical(group, split))) { stop("Grouping and split variables are identical.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Numeric Variables ####

    x <- x |>
      (\(y) !vapply(y, is.numeric, FUN.VALUE = logical(1L)))() |>
      (\(z) if (isTRUE(any(z))) {

        warning(paste0("Non-numeric variables were excluded from the analysis: ", paste(names(which(z)), collapse = ", ")), call. = FALSE)

        return(x[, -which(z), drop = FALSE])

      } else {

        return(x)

      })()

  if (isTRUE(ncol(x) == 0L)) { stop("No variables left for analysis after excluding non-numeric variables.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise deletion ####

  # Check inputs 'na.omit'
  .check.input(logical = "na.omit", envir = environment(), input.check = check)

  if (isTRUE(na.omit && any(is.na(x)))) {

    assign("x", na.omit(x)) |> (\(y) warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(y)$na.action)), call. = FALSE))()

    if (isTRUE(!is.null(group))) { group <- group[-attributes(na.omit(x))$na.action] }
    if (isTRUE(!is.null(split))) { split <- split[-attributes(na.omit(x))$na.action] }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("sample", "fisher", "point", "ci", "line", "append", "output"),
               numeric = list(seed = 1L, maxtol = 1L, nudge = 1L, point.size = 1L, point.shape  = 1L, errorbar.width = 1L, dodge.width = 1L, bins = 1L, density.linewidth = 1L, point.linewidth = 1L, ci.linewidth = 1L, intercept = 1L, xlim = 2L, ylim = 2L, axis.title.size = 1L, axis.text.size = 1L, strip.text.size = 1L, plot.margin = 4L, legend.box.margin = 4L, facet.ncol = 1L, facet.nrow = 1L, width = 1L, height = 1L, dpi = 1L),
               character = list(title = 1L, subtitle = 1L, legend.title = 1L),
               s.character = list(method = c("pearson", "spearman", "kendall-b", "kendall-c"), adjust = c("none", "joint", "approx"), se = c("fisher", "fieller", "bonett", "rin"), boot = c("none", "norm", "basic", "perc", "bc", "bca"), plot = c("none", "ci", "boot")),
               args = c("R", "alternative", "conf.level", "digits", "hist.alpha", "linetype", "units", "legend.position", "facet.scales", "write2"), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(isTRUE(check))) {

    # Check input 'adjust'
    if (isTRUE(all(method != "pearson"))) { if (isTRUE(all(adjust != "none"))) { warning("Argument 'adjust' is not applicable when 'method' is not \"pearson\"", call. = FALSE)  } }

    # Check input 'se'
    if (isTRUE(all(method == "pearson") || all(c("pearson", "spearman", "kendall-b", "kendall-c") %in% method))) { if (isTRUE(all(se != "fisher"))) { warning("Argument 'se' is not applicable when 'method' is \"pearson\".", call. = FALSE) } }

    if (isTRUE(all(method %in% c("kendall-b", "kendall-c")))) { if (isTRUE(all(se != "fieller"))) { warning("Argument 'se' is not applicable when 'method' is \"kendall-b\" or \"kendall-c\".", call. = FALSE) } }

    # Check input 'maxtol'
    if (isTRUE(maxtol <= 0 || maxtol > 0.1)) { stop("Please specify a number between 0 and 0.1 for the argument 'maxtol'.", call. = FALSE) }

    # Check input 'nudge'
    if (isTRUE(nudge <= 0 || nudge > 0.1)) { stop("Please specify a number between 0 and 0.1 for the argument 'nudge'.", call. = FALSE) }

    # Check input 'group'
    if (isTRUE(!is.null(group))) {

      # Input 'group' completely missing
      if (isTRUE(all(is.na(group)))) { stop("The grouping variable specified in 'group' is completely missing.", call. = FALSE) }

      # Only one group in 'group'
      if (isTRUE(length(na.omit(unique(unlist(group)))) == 1L)) { warning("There is only one group represented in the grouping variable specified in 'group'.", call. = FALSE) }

    }

    # Check input 'split'
    if (isTRUE(!is.null(split))) {

      # Input 'split' completely missing
      if (isTRUE(all(is.na(split)))) { stop("The grouping variable specified in 'split' is completely missing.", call. = FALSE) }

      # Only one split in 'split'
      if (isTRUE(length(na.omit(unique(unlist(split)))) == 1L)) { warning("There is only one split represented in the grouping variable specified in 'split'.", call. = FALSE) }

    }

    # Check input 'plot'
    if (isTRUE(all(plot == "boot") && (all(boot == "none") || all(c("none", "norm", "basic", "stud", "perc", "bc", "bca") %in% boot)))) { stop("Please request bootstrap confidence intervals by specifying the 'boot' argument to plot bootstrap samples.", call. = FALSE) }

    # Check input 'intercept'
    if (isTRUE(!is.numeric(intercept) || length(intercept) != 1L || intercept > 1L || intercept < -1L)) { stop("Please specify a numeric value between -1 and 1 for the argument 'intercept'.", call. = FALSE) }

    # Check input 'group.col'
    if (isTRUE(!is.null(group.col) && length(group.col) != length(unique(group)))) { stop(paste0("Please specify a character vector with ", length(unique(group)), " elements for the argument 'group.col'."), call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'method' Argument ####

  method <- ifelse(all(c("pearson", "spearman", "kendall-b", "kendall-c") %in% method), "pearson", method)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Non-Normality Adjustment Method ####

  if (isTRUE(method == "pearson")) { adjust <- ifelse(all(c("none", "joint", "approx") %in% adjust), "approx", adjust) } else { adjust <- "none" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'se' Argument ####

  if (isTRUE(method != "pearson")) { se <- ifelse(all(c("fisher", "fieller", "bonett", "rin") %in% se), "fieller", se) } else if (isTRUE(method %in% c("kendall-b", "kendall-c"))) { se <- "fieller" } else { se <- "fisher" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'boot' Argument ####

  boot <- ifelse(all(c("none", "norm", "basic", "perc", "bc", "bca") %in% boot), "none", boot)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'alternative' Argument ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'plot' Argument ####

  plot <- ifelse(all(c("none", "ci", "boot") %in% plot), "none", plot)

  # Package ggplot2
  if (isTRUE(check && plot != "none")) { if (isTRUE(!nzchar(system.file(package = "ggplot2")))) { stop("Package \"ggplot2\" is needed to draw a plot, please install the package.", call. = FALSE) } }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'xlab', 'ylab', 'xlim', 'ylim', 'xbreaks', and 'ybreaks' Argument ####

  switch(plot, "ci" = {

    ylab <- if (isTRUE(is.null(ylab))) { "Correlation Coefficient" } else { ylab }
    ylim <- if (isTRUE(is.null(ylim))) { c(-1, 1) } else { ylim }

  }, "boot" = {

    xlab <- if (isTRUE(is.null(xlab))) { "Correlation Coefficient" } else { xlab }
    ylab <- if (isTRUE(is.null(ylab))) { "Probability Density, f(x)" } else { ylab }
    xlim <- if (isTRUE(is.null(xlim))) { c(-1, 1) } else { xlim }

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

  low <- upp <- point <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, No Split ####

  if (isTRUE(is.null(group) && is.null(split))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No Bootstrapping ####

    if (isTRUE(boot == "none")) {

      #...................
      ### Product-Moment Correlation Coefficient ####

      switch(method, "pearson" = {

        result <- do.call("rbind",
                          apply(as.matrix(combn(seq_len(ncol(x)), m = 2L)), 2L, function(y) x |>
                                  (\(z) suppressWarnings(.ci.pearson.cor.adjust(z[, y[1L]], z[, y[2L]], adjust = adjust, alternative = alternative, conf.level = conf.level, sample = sample, seed = seed, maxtol = maxtol, nudge = nudge)))() |>
                                  (\(w) data.frame(var1 = colnames(x)[y[1L]], var2 = colnames(x)[y[2L]], n = w$n, nNA = w$nNA, pNA = w$pNA, skew1 = w$skew1, kurt1 = w$kurt1, skew2 = w$skew2, kurt2 = w$kurt2, cor = w$cor, low = w$ci[1L, adjust], upp = w$ci[2L, adjust], row.names = NULL))()))

      # Skewness > 4.4 or kurtosis > 43.4
      if (isTRUE(adjust == "approx" && any(!is.na(result$cor) & (abs(result$skew1) > 4.4 | abs(result$skew1) > 4.4 | abs(result$kurt1) > 43.4 | abs(result$kurt2) > 43.4)))) { warning(paste0("Variables with |skewness| above 4.4 or |kurtosis| above 43.4 may lead to coverage rates lower than ", conf.level, "."), call. = FALSE) }

      #...................
      ### Spearman Correlation Coefficient ####

      }, "spearman" = {

        result <- do.call("rbind",
                          apply(as.matrix(combn(seq_len(ncol(x)), m = 2L)), 2L, function(y) x |>
                                  (\(z) suppressWarnings(.ci.spearman.cor.se(z[, y[1L]], z[, y[2L]], se = se, alternative = alternative, conf.level = conf.level, sample = sample)))() |>
                                  (\(w) data.frame(var1 = colnames(x)[y[1L]], var2 = colnames(x)[y[2L]], n = w$n, nNA = w$nNA, pNA = w$pNA, skew1 = w$skew1, kurt1 = w$kurt1, skew2 = w$skew2, kurt2 = w$kurt2, cor = w$cor, low = w$ci[1L], upp = w$ci[2L], row.names = NULL))()))

        # n <= 10 or |rs| >= 0.8
        if (isTRUE(se == "fieller" && any(!is.na(result$cor) & (result$n <= 10L | abs(result$cor) >= 0.8)))) { warning(paste0("Sample size n <= 10 or absolute correlation rs >= 0.8 may lead to coverage rates lower than ", conf.level, "."), call. = FALSE) }

        # |rs| >= 0.9
        if (isTRUE(se == "bonett" && any(!is.na(result$cor) & (abs(result$cor) > 0.9)))) { warning(paste0("Absolute correlation rs > 0.9 may lead to coverage rates lower than ", conf.level, "."), call. = FALSE) }

      #...................
      ### Kendall's Tau-b ####

      }, "kendall-b" = {


        result <- do.call("rbind",
                          apply(as.matrix(combn(seq_len(ncol(x)), m = 2L)), 2L, function(y) x |>
                                  (\(z) suppressWarnings(.ci.kendall.b(z[, y[1L]], z[, y[2L]], alternative = alternative, conf.level = conf.level, sample = sample)))() |>
                                  (\(w) data.frame(var1 = colnames(x)[y[1L]], var2 = colnames(x)[y[2L]], n = w$n, nNA = w$nNA, pNA = w$pNA, skew1 = w$skew1, kurt1 = w$kurt1, skew2 = w$skew2, kurt2 = w$kurt2, cor = w$tau, low = w$ci[1L], upp = w$ci[2L], row.names = NULL))()))

        # n <= 10 or |rs| >= 0.8
        if (isTRUE(any(!is.na(result$cor) & (result$n <= 10L | abs(result$cor) >= 0.8)))) { warning(paste0("Sample size n <= 10 or absolute correlation tau >= 0.8 may lead to coverage rates lower than ", conf.level, "."), call. = FALSE) }

      #...................
      ### Kendall's Tau-c ####

      }, "kendall-c" = {

        result <- do.call("rbind",
                          apply(as.matrix(combn(seq_len(ncol(x)), m = 2L)), 2L, function(y) x |>
                                  (\(z) suppressWarnings(.ci.kendall.c(z[, y[1L]], z[, y[2L]], alternative = alternative, conf.level = conf.level, sample = sample)))() |>
                                  (\(w) data.frame(var1 = colnames(x)[y[1L]], var2 = colnames(x)[y[2L]], n = w$n, nNA = w$nNA, pNA = w$pNA, skew1 = w$skew1, kurt1 = w$kurt1, skew2 = w$skew2, kurt2 = w$kurt2, cor = w$tau, low = w$ci[1L], upp = w$ci[2L], row.names = NULL))()))

        # n <= 10 or |rs| >= 0.8
        if (isTRUE(any(!is.na(result$cor) & (result$n <= 10L | abs(result$cor) >= 0.8)))) { warning(paste0("Sample size n <= 10 or absolute correlation tau >= 0.8 may lead to coverage rates lower than ", conf.level, "."), call. = FALSE) }

      })

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bootstrapping ####

    } else {

      result.boot <- apply(as.matrix(combn(seq_len(ncol(x)), m = 2L)), 2L, function(y) x |>
                        (\(z) suppressWarnings(.ci.boot.cor(data = z, x = y[1L], y = y[2L], method = method, boot = boot, R = R, fisher = fisher, alternative = alternative, conf.level = conf.level, seed = seed)))() |>
                        (\(w) list(t = data.frame(var1 = colnames(x)[y[1L]], var2 = colnames(x)[y[2L]], cor = w$t), result = data.frame(var1 = colnames(x)[y[1L]], var2 = colnames(x)[y[2L]], n = w$n, nNA = w$nNA, pNA = w$pNA, skew1 = w$skew1, kurt1 = w$kurt1, skew2 = w$skew2, kurt2 = w$kurt2, cor = w$cor, low = w$ci[1L], upp = w$ci[2L], row.names = NULL)))())

      boot.sample <- do.call("rbind", lapply(result.boot, function(y) y$t))
      result <- do.call("rbind", lapply(result.boot, function(y) y$result))

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, No Split ####

  } else if (isTRUE(!is.null(group) && is.null(split))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No Bootstrapping ####

    if (isTRUE(boot == "none")) {

      result <- lapply(split(x, f = group), function(y) misty::ci.cor(y, group = NULL, split = NULL, method = method, adjust = adjust, se = se, sample = sample, seed = seed, maxtol = maxtol, nudge = nudge, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)$result) |>
        (\(z) data.frame(group = rep(names(z), each = unique(unlist(lapply(z, nrow)))), do.call("rbind", z), row.names = NULL))()

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bootstrapping ####

    } else {

      result.boot <- lapply(split(x, f = group), function(y) misty::ci.cor(y, group = NULL, split = NULL, method = method, sample = sample, seed = seed, boot = boot, R = R, alternative = alternative, conf.level = conf.level, na.omit = FALSE, check = FALSE, output = FALSE)) |>
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

      result <- lapply(split(x, f = split), function(y) misty::ci.cor(y, group = NULL, split = NULL, method = method, adjust = adjust, se = se, sample = sample, seed = seed, maxtol = maxtol, nudge = nudge, alternative = alternative, conf.level = conf.level, na.omit = FALSE, as.na = NULL, check = FALSE, output = FALSE)$result)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bootstrapping ####

    } else {

      result.boot <- lapply(split(x, f = split), function(y) misty::ci.cor(y, group = NULL, split = NULL, method = method, sample = sample, seed = seed, boot = boot, R = R, alternative = alternative, conf.level = conf.level, na.omit = FALSE, check = FALSE, output = FALSE)) |>
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

      result <- lapply(split(data.frame(x, group = group), f = split), function(y) misty::ci.cor(y[, -grep("group", names(y))], group = y$group, split = NULL, method = method, adjust = adjust, se = se, sample = sample, seed = seed, maxtol = maxtol, nudge = nudge, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)$result)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Bootstrapping ####

    } else {

      result.boot <- lapply(split(data.frame(x, group = group), f = split), function(y) misty::ci.cor(y[, -grep("group", names(y))], group = y$group, split = NULL, method = method, sample = sample, seed = seed, boot = boot, R = R, alternative = alternative, conf.level = conf.level, na.omit = FALSE, check = FALSE, output = FALSE)) |>
        (\(z) list(boot.sample = data.frame(split = rep(names(z), each = R*unique(unlist(lapply(z, function(q) nrow(q$result))))), do.call("rbind", lapply(z, function(w) w$boot)), row.names = NULL), result = lapply(z, function(w) w$result)))()

      boot.sample <- result.boot$boot.sample
      result <- result.boot$result

    }

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "ci.cor",
                 data = list(x = x, group = group, split = split),
                 args = list(method = method, adjust = adjust, se = se, sample = sample, seed = seed, maxtol = maxtol, nudge = nudge, boot = boot, R = R, fisher = fisher, alternative = alternative, conf.level = conf.level, na.omit = na.omit, digits = digits, as.na = as.na, plot = plot, point.size = point.size, point.shape = point.shape, errorbar.width = errorbar.width, dodge.width = dodge.width, hist = hist, binwidth = binwidth, bins = bins, hist.alpha = hist.alpha, fill = fill, density = density, density.col = density.col, density.linewidth = density.linewidth, density.linetype = density.linetype, point = point, point.col = point.col, point.linewidth = point.linewidth, point.linetype = point.linetype, ci = ci, ci.col = ci.col, ci.linewidth = ci.linewidth, ci.linetype = ci.linetype, line = line, intercept = intercept, linetype = linetype, line.col = line.col, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks, axis.title.size = axis.title.size, axis.text.size = axis.text.size, strip.text.size = strip.text.size, title = title, subtitle = subtitle, group.col = group.col, plot.margin = plot.margin, legend.title = legend.title, legend.position = legend.position, legend.box.margin = legend.box.margin, facet.ncol = facet.ncol, facet.nrow = facet.nrow, facet.scales = facet.scales, filename = filename, width = width, height = height, units = units, dpi = dpi, write = write, append = append, check = check, output = output),
                 boot = if (isTRUE(boot != "none")) { boot.sample } else { NULL },
                 plot = NULL, result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Plot and Save Results ------------------------------------------------------

  if (isTRUE(plot != "none")) { object$plot <- plot(object, filename = filename, width = width, height = height, units = units, dpi = dpi, check = FALSE) |> (\(y) suppressMessages(suppressWarnings(print(y))))() }

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) { .write.result(object = object, write = write, append = append) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
