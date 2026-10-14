\name{cor.matrix}
\alias{cor.matrix}
\title{Correlation Matrix}
\usage{
cor.matrix(data, ...,
           method = c("pearson", "spearman", "kendall-b", "kendall-c", "tetra", "poly"),
           group = NULL, exact = FALSE, continuity = TRUE, ml = FALSE,
           print = c("all", "cor", "n", "stat", "df", "p"),
           tri = c("both", "lower", "upper"), alpha = 0.05,
           color = "default", style = c("regular", "bold", "italic"),
           p.adj = "none", na.omit = FALSE, digits = 2, p.digits = 3,
           as.na = NULL,  plot = FALSE, geom = c("none", "label"),
           filename = NULL, width = NA, height = NA, dpi = 600,
           write = NULL, append = TRUE, check = TRUE, output = TRUE)
}
\arguments{
\item{data}{a data frame with numeric variables, i.e., factors and character
variables are excluded from \code{data} before conducting the
analysis.}

\item{...}{an expression indicating the variable names in \code{data},
e.g., \code{cor.matrix(dat, x1, x2, x3)}. Note that the
operators \code{+}, \code{-}, \code{~}, \code{:},
\code{::}, and \code{!} can also be used to select variables,
see 'Details' in the \code{\link{df.subset}} function.}

\item{method}{a character vector indicating which correlation coefficient
is to be computed, i.e. \code{"pearson"} for Pearson product-moment correlation
coefficient (default), \code{"spearman"} for Spearman's rank-order correlation
coefficient, \code{"kendall-b"} for Kendall's Tau-b correlation coefficient,
\code{"kendall-c"} for Kendall-Stuart's Tau-c correlation coefficient,
\code{"tetra"} for tetrachoric correlation coefficient, and \code{"poly"} for
polychoric correlation coefficient.}

\item{group}{either a character string indicating the variable name of
the grouping variable in \code{data}, or a
vector representing the grouping variable. Note that the
grouping variable is limited to two groups.}

\item{exact}{logical: if \code{TRUE} (default), an exact p-value is computed
for Spearman's rank-order correlation coefficient and Kendall's
Kendall's Tau-b correlation coefficient. Note that the exact
p-value is not available in the presence of ties.}

\item{continuity}{logical: if \code{TRUE} (default), continuity correction is
used for testing Spearman's rank-order correlation coefficient
and Kendall's Tau-b correlation.}

\item{ml}{logical: if \code{FALSE} (default), a two-step approximation
is used to compute the tetrachoric and polychoric correlation
coefficient, while the maximum-likelihood (ML) estimate is
computed if \code{TRUE}. Note that ML estimation is computationally
expensive, i.e., takes a lot of time.}

\item{print}{a character vector indicating which results
to show on the console, i.e. \code{"all"} for all results,
\code{"cor"} for correlation coefficients, \code{"n"} for the
sample sizes, \code{"stat"} for the test statistic, \code{"df"}
for the degrees of freedom, and \code{"p"} for \emph{p}-values.}

\item{tri}{a character string indicating which triangular of the matrix
to show on the console, i.e., \code{both} for upper and lower
triangular, \code{lower} (default) for the lower triangular,
and \code{upper} for the upper triangular.}

\item{alpha}{a numeric value between 0 and 1 indicating the significance
level at which correlation coefficients are are highlighted when
specifying the argument \code{color}.}

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

\item{p.adj}{a character string indicating an adjustment method for multiple
testing based on \code{\link{p.adjust}}, i.e., \code{none} (default),
\code{bonferroni}, \code{holm}, \code{hochberg},
\code{hommel}, \code{BH}, \code{BY}, or \code{fdr}.
See help page of the \code{\link{p.adjust}} function.}

\item{na.omit}{logical: if \code{TRUE}, incomplete cases are removed before
conducting the analysis (i.e., listwise deletion); if \code{FALSE}
(default), pairwise deletion is used.}

\item{digits}{an integer value indicating the number of decimal places to be
used for displaying correlation coefficients.}

\item{p.digits}{an integer value indicating the number of decimal places to be
used for displaying \emph{p}-values.}

\item{as.na}{a numeric vector indicating user-defined missing values,
i.e. these values are converted to \code{NA} before conducting
the analysis.}

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
  \item{\code{data}}{data frame used for the current analysis}
  \item{\code{args}}{specification of function arguments}
  \item{\code{result}}{list with result tables, i.e., \code{cor} for the
                       correlation matrix, \code{n} for a matrix with the sample
                       sizes, \code{stat} for a matrix with the test statistics,
                       \code{df} for a matrix with the degrees of freedom, and
                       \code{p}-value for the matrix with the significance values
                       (\emph{p}-values)}
}
\description{
This function computes a correlation matrix based on Pearson product-moment
correlation coefficient, Spearman's rank-order correlation coefficient,
Kendall's Tau-b correlation coefficient, Kendall-Stuart's Tau-c correlation
coefficient, tetrachoric correlation coefficient, or polychoric correlation
coefficient and computes significance values (\emph{p}-values) for testing the
two-sided hypothesis H0: \eqn{\rho} = 0 for all pairs of variables. Statistically
significant correlations can be highlighted by specifying the argument \code{color}.
}
\note{
This function uses a modified copy of the \code{polychor()} function in the
\pkg{polycor} package by John Fox when requesting tetrachoric or polychoric
correlation coefficients.
}
\examples{
\dontrun{

#————————————————————————————————————————————————————————————————————————————
# Pearson Product-Moment Correlation Coefficient

# Example 1a: Pearson product-moment correlation matrix using pairwise deletion
cor.matrix(airquality, Ozone:Wind)

# Example 1b:  Alternative specification without using the '...' argument
cor.matrix(airquality[, c("Ozone", "Solar.R", "Wind")])

# Example 2a: Highlight statistically significant result in bright red
cor.matrix(airquality, Ozone, Solar.R, Wind, color = "b.red")

# Example 2b: Highlight statistically significant result in boldface
cor.matrix(airquality, Ozone, Solar.R, Wind, color = "black", style = "bold")

# Example 3a: Print sample size, degrees of freedom, and significance values
cor.matrix(airquality, Ozone, Solar.R, Wind, print = "all")

# Example 3b: Listwise deletion
cor.matrix(airquality, Ozone, Solar.R, Wind, na.omit = TRUE)

# Example 3c: Significance values with Bonferroni correction
cor.matrix(airquality, Ozone, Solar.R, Wind, print = "all", p.adj = "bonferroni")

#————————————————————————————————————————————————————————————————————————————
# Spearman's Rank-Order Correlation Coefficient and Kendall's Tau

# Example 4a: Spearman's rank-order correlation matrix
cor.matrix(airquality, Ozone, Solar.R, Wind, method = "spearman")

# Example 4b: Kendall's Tau-c
cor.matrix(airquality, Ozone, Solar.R, Wind, method = "kendall-c")

#————————————————————————————————————————————————————————————————————————————
# Tetrachoric and Polychoric Correlation Coefficient

# Example 5a: Tetrachoric correlation matrix
cor.matrix(data.items, +ditem, method = "tetra")

# Example 5b: Polychoric correlation matrix
cor.matrix(data.items, +pitem, method = "poly")

#————————————————————————————————————————————————————————————————————————————
# Grouping Variable

# Example 6a: Results for group 'vs' = "0" and "1" separately
cor.matrix(mtcars, mpg:disp, group = "vs")

# Example 6b: Alternative specification without using the '...' argument
cor.matrix(mtcars[, c("mpg", "cyl", "disp")], group = mtcars$vs)

#————————————————————————————————————————————————————————————————————————————
# Plot

# Example 7a: Correlation heat map
cor.matrix(mtcars, plot = TRUE)

# Example 7b: Correlation heat map or group 'vs' = "0" and "1" separately
cor.matrix(mtcars, group = "vs", plot = TRUE)

# Example 7c: Highlight statistically significant result in blue
cor.matrix(mtcars, color = "blue", plot = TRUE)

# Example 7d: Highlight statistically significant result in boldface
cor.matrix(mtcars, color = "black", style = "bold", plot = TRUE)

# Example 7e: Plot results using the plot() function, use additional arguments
# see Details in the help page of the function plot.misty.object
object <- cor.matrix(mtcars)
plot(object, scale.fill.low = "#DC267F", scale.fill.high = "#009E73",
     text.col = c("gray80", "red"), text.size = 4.5, axis.text.size = 13)

#————————————————————————————————————————————————————————————————————————————
# Write Results

# Example 8a: Write Results into a text file
cor.matrix(airquality, Ozone, Solar.R, Wind, print = "all", write = "Correlation.txt")

# Example 8b: Write Results into an Excel file
cor.matrix(airquality, Ozone, Solar.R, Wind, print = "all", write = "Correlation.xlsx")

# Example 8c: Save plot as PNG fine
cor.matrix(mtcars, plot = TRUE, filename = "Correlation.png", width = 8, height = 7)
}
}
\references{
Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
- Using R and SPSS}. John Wiley & Sons.

Revelle, W. (2018) \emph{psych: Procedures for personality and psychological
research}. Northwestern University, Evanston, Illinois, USA,
https://CRAN.R-project.org/package=psych Version = 1.8.12.
}
\seealso{
\code{\link{write.result}}, \code{\link{cohens.d}}, \code{\link{effsize}},
\code{\link{multilevel.cor}}, \code{\link{multilevel.icc}}, \code{\link{na.auxiliary}}
}
\author{
Takuya Yanagida
}
