\name{multilevel.cor}
\alias{multilevel.cor}
\title{Within-Group and Between-Group Correlation Matrix}
\usage{
multilevel.cor(data, ..., cluster, estimator = c("ML", "MLR"), constr.var = FALSE,
               optim.method = c("nlminb", "em"), optim.switch = TRUE,
               print = c("all", "summary", "cor", "se", "stat", "p"), split = FALSE,
               order = FALSE, tri = c("both", "lower", "upper"), tri.lower = TRUE,
               missing = c("listwise", "fiml"), alpha = 0.05,
               color = "default", style = c("regular", "bold", "italic"),
               p.adj = "none", digits = 2, p.digits = 3, as.na = NULL,
               plot = FALSE, geom = c("none", "label"), filename = NULL,
               width = NA, height = NA, dpi = 600, write = NULL, append = TRUE,
               check = TRUE, output = TRUE)
}
\arguments{
\item{data}{a data frame.}

\item{...}{an expression indicating the variable names in \code{data},
e.g., \code{multilevel.cor(dat, x1, x2, x3)}. Note that
the operators \code{+}, \code{-}, \code{~},
\code{:}, \code{::}, and \code{!} can also be used to
select variables, see 'Details' in the \code{\link{df.subset}}
function.}

\item{cluster}{either a character string indicating the variable name of
the cluster variable in \code{data}, or a
vector representing the nested grouping structure (i.e.,
group or cluster variable).}

\item{estimator}{a character string indicating the estimator to be used, i.e.,
\code{"ML"} for maximum likelihood with conventional
standard errors and \code{"MLR"} for maximum likelihood with
Huber-White robust standard errors. The default setting
depends on the argument \code{color}, i.e., \code{"ML"} is
used when specifying \code{color = "default"} (default) and
\code{"MLR"} is used when specifying a color for the
argument \code{color}.}

\item{constr.var}{logical: if \code{TRUE}, inequality constraints are imposed
for the variance parameters at the between level, i.e., variances are constrained
to be greater than 0.}

\item{optim.method}{a character string indicating the optimizer, i.e.,
\code{"nlminb"} for the unconstrained and
bounds-constrained quasi-Newton method optimizer and
\code{"em"} for the Expectation Maximization (EM) algorithm.
By default, the optimizer \code{"nlminb"} is used with
complete data or when using listwise deletion for missing
data handling, while the optimizer \code{"em"} is used
when using for full information maximum likelihood (FIML)
method for missing data handling.}

\item{optim.switch}{logical: if \code{TRUE} (default), model estimation switches
to Expectation Maximization (EM) algorithm (\code{"em"})
if the quasi-Newton optimization (\code{"nlminb"})
does not converge.}

\item{print}{a character vector indicating which
results to show on the console, i.e. \code{"all"} for all
results, \code{"summary"} for a summary of the specification
of the estimation method and missing data handling in lavaan,
\code{"cor"} (default) for correlation coefficients, \code{"se"}
for standard errors, \code{"stat"} for z test statistics,
and \code{"p"} for \emph{p}-values. By default, the function
only prints the within-group and between-group correlation
matrix.}

\item{split}{logical: if \code{TRUE}, output table is split in
within-group and between-group correlation matrix.}

\item{order}{logical: if \code{TRUE}, variables in the output table are
ordered, so that variables specified in the argument
\code{between} are shown first.}

\item{tri}{a character string indicating which triangular of the
matrix to show on the console when \code{split = TRUE},
i.e., \code{both} for upper and \code{upper} for the upper
triangular.}

\item{tri.lower}{logical: if \code{TRUE} (default) and \code{split = FALSE}
(default), within-group correlations are shown in the lower
triangular and between-group correlation are shown in the
upper triangular.}

\item{missing}{a character string indicating how to deal with missing
data, i.e., \code{"listwise"} for listwise deletion or
\code{"fiml"} (default) for full information maximum
likelihood (FIML) method. Note that it takes longer to
estimate models while using FIML and using FIML is
prone to issues with model convergence, these issues might
be resolved by switching to listwise deletion.}

\item{alpha}{a numeric value between 0 and 1 indicating the significance
level at which correlation coefficients are printed
boldface when when specifying the argument \code{color}.}

\item{color}{a character string indicating the text color for highlighting
statistically significant correlation coefficients , i.e.,
\code{"default"} (default) for the default text color without
color coding and various text colors for highlighting like
\code{"red"}, \code{"b.red"}, \code{"green"}, \code{"b.green"},
\code{"blue"}, or \code{"b.blue"}, see the help page of the
\code{\link{chr.color}} function. Note that this option is
only supported on the RStudio platform, i.e., the argument
switches to \code{"default"} when using R Markdown and when
writing the output into a text file (\code{.txt}).}

\item{style}{a character vector indicating the font style for
statistically significant correlation coefficients, i.e.,
\code{"regular"} (default) for regular text, \code{"bold"}
for bold text, and \code{"italic"} for italic text. Note
that the font style \code{"bold"} and \code{"italic"} can
be combined, i.e., style = c("bold", "italic") provides a
bold and italic text. Note that the argument \code{color}
needs to be specified to change the style of the text, e.g.
\code{color = "black"} and \code{style = "bold"} to for
bold text.}

\item{p.adj}{a character string indicating an adjustment method for
multiple testing based on \code{\link{p.adjust}}, i.e.,
\code{none} (default), \code{bonferroni}, \code{holm},
\code{hochberg}, \code{hommel}, \code{BH}, \code{BY}, or
\code{fdr}. See help page of the \code{\link{p.adjust}} function.}

\item{digits}{an integer value indicating the number of decimal places
to be used for displaying correlation coefficients.}

\item{p.digits}{an integer value indicating the number of decimal places
to be used for displaying \emph{p}-values.}

\item{as.na}{a numeric vector indicating user-defined missing values,
i.e. these values are converted to \code{NA} before
conducting the analysis. Note that \code{as.na()} function
is only applied to \code{data} but not to \code{cluster}.}

\item{plot}{logical: if \code{TRUE},a  correlation heat map is drawn.
For additional plotting arguments, see \code{Details} in
the help page of the function \code{plot.misty.object}.}

\item{geom}{character vector indicating which plot elements to draw,
i.e., \code{"none"} for not printing correlation coefficients
inside every tile, \code{"label"} for printing correlation
coefficients inside every tile.}

\item{filename}{a character string indicating the \code{filename}
argument including the file extension in the \code{ggsave}
function. Note that one of \code{".eps"}, \code{".ps"},
\code{".tex"}, \code{".pdf"} (default),
\code{".jpeg"}, \code{".tiff"}, \code{".png"},
\code{".bmp"}, \code{".svg"} or \code{".wmf"} needs
to be specified as file extension in the \code{file}
argument. Note that plots can only be saved when
\code{plot = TRUE}.}

\item{width}{a numeric value indicating the \code{width} argument
(default is the size of the current graphics device)
for the \code{ggsave} function.}

\item{height}{a numeric value indicating the \code{height} argument
(default is the size of the current graphics device)
for the \code{ggsave} function.}

\item{dpi}{a numeric value indicating the \code{dpi} argument
(default is \code{600}) for the \code{ggsave} function.}

\item{write}{a character string naming a file for writing the output into
either a text file with file extension \code{".txt"} (e.g.,
\code{"Output.txt"}) or Excel file with file extension
\code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
name does not contain any file extension, an Excel file will
be written.}

\item{append}{logical: if \code{TRUE} (default), output will be appended
to an existing text file with extension \code{.txt} specified
in \code{write}, if \code{FALSE} existing text file will be
overwritten.}

\item{check}{logical: if \code{TRUE} (default), argument specification is checked.}

\item{output}{logical: if \code{TRUE} (default), output is shown on the console.}
}
\value{
Returns an object of class \code{misty.object}, which is a list with following
entries:
  \item{\code{call}}{function call}
  \item{\code{type}}{type of analysis}
  \item{\code{data}}{data frame specified in \code{data} including the group variable
                     specified in \code{cluster}}
  \item{\code{args}}{specification of function arguments}
  \item{\code{model.fit}}{fitted lavaan object (\code{mod.fit})}
  \item{\code{result}}{list with result tables, i.e., \code{summary} for the
                       specification of the estimation method and missing data
                       handling in lavaan, \code{wb.cor} for the within- and
                       between-group correlations, \code{wb.se} for the standard
                       error of the within- and between-group correlations,
                       \code{wb.stat} for the test statistic of within- and between-group
                       correlations, \code{wb.p} for the significance value of
                       the within- and between-group correlations, \code{with.cor}
                       for the within-group correlations, \code{with.se} for the
                       standard error of the within-group correlations, \code{with.stat}
                       for the test statistic of within-group correlations, \code{with.p}
                       for the significance value of the within-group correlations,
                       \code{betw.cor} for the between-group correlations, \code{betw.se}
                       for the standard error of the between-group correlations,
                       \code{betw.stat} for the test statistic of between-group
                       correlations, \code{betw.p} for the significance value of
                       the between-group correlations}
}
\description{
This function computes the within-group and between-group correlation matrix by
calling the \code{sem} function in the R package \pkg{lavaan} and provides standard
errors, z test statistics, and significance values (\emph{p}-values) for testing
the hypothesis H0: \eqn{\rho} = 0 for all pairs of variables within and between
groups. By default, the function provides the within-group and
between-group correlation matrix. Statistically significant correlations can
be highlighted by specifying the argument \code{color}.
}
\details{
\describe{
\item{\strong{Within-Group and Between-Group Variables}}{The function automatically
identifies (1) variables in the data frame specified in \code{data} that are
measured at the individual level and modeled only at the within level and (2)
variables in the data frame specified in \code{data} that are measured at the
cluster level and modeled only at the between level. The former variables have
no variance in the between part of the model (i.e., ICC(1) is 0 e.g. due to centering
within clusters), while the latter variables do not have any variance within clusters.}
\item{\strong{Estimation Method}}{The default setting
for the argument \code{estimator} is depending on the setting of the argument
\code{color}. If \code{color = "default"} (default), maximum likelihood estimation
(\code{estimator = "ML"}) is used, while maximum likelihood with Huber-White
robust standard errors (\code{estimator = "MLR"}) that are robust against
non-normality is used when when specifying a color for the argument \code{color}.}
\item{\strong{Missing Data Handling}}{In the presence of missing data, full information
maximum likelihood (FIML) method (\code{missing = "fiml"}) is used by default.
Note that FIML method cannot deal with within-group variables that have no
variance within some clusters. In this cases, the function will switch to
listwise deletion. Using FIML method might result in issues with model convergence,
which might be resolved by switching to listwise deletion (\code{missing = "listwise"}).}
\item{\strong{Optimizer}}{The lavaan package uses a quasi-Newton optimization
method (\code{"nlminb"}) by default. If the optimizer does not converge, model
estimation switches to the Expectation Maximization (EM) algorithm (\code{"em"})
if the argument \code{optim.switch} is specified as \code{TRUE} (default).}
\item{\strong{Statistical Significance}}{Statistically significant correlation
coefficients can be shown color coded on the console by specifying the argument
\code{color}. However, this option is not supported when using R Markdown.}
\item{\strong{Adjustment Method for Multiple Testing }}{Adjustment method for
multiple testing when specifying the argument \code{p.adj} is applied to
the within-group and between-group correlation matrix separately.}
}
}
\note{
The function uses the functions \code{sem}, \code{lavInspect},
\code{lavMatrixRepresentation}, \code{lavTech}, \code{parameterEstimates},
and \code{standardizedsolution} provided in the R package \pkg{lavaan} by
Yves Rosseel (2012).
}
\examples{
\dontrun{

# Load data set "Demo.twolevel" in the lavaan package
data("Demo.twolevel", package = "lavaan")

#————————————————————————————————————————————————————————————————————————————
# Cluster Variable Specification

# Example 1a: Specification using the argument '...'
multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster")

# Example 1b: Alternative specification with cluster variable 'cluster' in 'data'
multilevel.cor(Demo.twolevel[, c("y1", "y2", "y3", "cluster")], cluster = "cluster")

# Example 1c: Alternative specification with cluster variable 'cluster' not in 'data'
multilevel.cor(Demo.twolevel[, c("y1", "y2", "y3")], cluster = Demo.twolevel$cluster)

#————————————————————————————————————————————————————————————————————————————
# Arguments 'color', 'style', 'split', 'print', and 'p.adj'

# Example 2a: Highlight statistically significant result in bright red
multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster", color = "b.red")

# Example 2b: Highlight statistically significant result in boldface
multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster", color = "black",
               style = "bold")

# Example 3: Split output table in within-group and between-group correlation matrix
multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster", split = TRUE,
               color = "green", style = "bold")

# Example 4a: Print summary of the lavaan specification and all results
multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster", print = "all")

# Example 4b: Print summary of the lavaan specification and correlation coefficients
multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster", print = c("summary", "cor"))

# Example 5: Significance values with Bonferroni correction
multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster", print = c("cor", "p"),
               p.adj = "bonferroni")

#————————————————————————————————————————————————————————————————————————————
# Variables Measured at the Within and Cluster Level

# Example 6a: Variables "y1", "y2", and "y2" modeled at both the within and between level
#             Variables "w1" and "w2" modeled at the cluster level
multilevel.cor(Demo.twolevel, y1, y2, y3, w1, w2, cluster = "cluster")

# Example 6b: Print cluster level variables first
multilevel.cor(Demo.twolevel, y1, y2, y3, w1, w2, cluster = "cluster", order = TRUE)

#————————————————————————————————————————————————————————————————————————————
# lavaan Model and Summary of the Estimated Model

# Example 7: lavaan model and summary of the multilevel model used to compute
#             the within-group and between-group correlation matrix
mod <- multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster", output = FALSE)

# lavaan model syntax
mod$model

# Fitted lavaan object
lavaan::summary(mod$model.fit, standardized = TRUE)

#————————————————————————————————————————————————————————————————————————————
# Plot

# Example 8a: Correlation heat map
multilevel.cor(Demo.twolevel, y1, y2, y3, w1, w2, cluster = "cluster", plot = TRUE)

# Example 8b: Split correlation heat map in within-p and between-group
multilevel.cor(Demo.twolevel, y1, y2, y3, w1, w2, cluster = "cluster",
               split = TRUE, plot = TRUE)

# Example 8c: Highlight statistically significant result in blue
multilevel.cor(Demo.twolevel, y1, y2, y3, w1, w2, cluster = "cluster",
               color = "blue", plot = TRUE)

# Example 8d: Highlight statistically significant result in boldface
multilevel.cor(Demo.twolevel, y1, y2, y3, w1, w2, cluster = "cluster",
                color = "black", style = "bold", plot = TRUE)

# Example 8e: Plot results using the plot() function, use additional arguments
# see Details in the help page of the function plot.misty.object
object <-  multilevel.cor(Demo.twolevel, y1, y2, y3, w1, w2, cluster = "cluster")
plot(object, scale.fill.low = "#DC267F", scale.fill.high = "#009E73",
     text.col = c("gray80", "red"), text.size = 4.5, axis.text.size = 13)

#————————————————————————————————————————————————————————————————————————————
# Write Results

# Example 9a: Write Results into a text file
multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster",
               write = "Multilevel_Correlation.txt")

# Example 9b: Write Results into a Excel file
multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster",
               write = "Multilevel_Correlation.xlsx")

# Example 9c: Save plot as PNG fine
multilevel.cor(Demo.twolevel, y1, y2, y3, cluster = "cluster", plot = TRUE,
               filename = "Multilevel_Correlation.png", width = 8, height = 7)
}
}
\references{
Hox, J., Moerbeek, M., & van de Schoot, R. (2018). \emph{Multilevel analysis:
Techniques and applications} (3rd. ed.). Routledge.

Snijders, T. A. B., & Bosker, R. J. (2012). \emph{Multilevel analysis: An
introduction to basic and advanced multilevel modeling} (2nd ed.). Sage
Publishers.
}
\seealso{
\code{\link{write.result}}, \code{\link{multilevel.descript}},
\code{\link{multilevel.icc}}, \code{\link{cluster.scores}}
}
\author{
Takuya Yanagida
}
