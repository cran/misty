\name{na.pattern}
\alias{na.pattern}
\title{Missing Data Pattern}
\usage{
na.pattern(data, ..., order = FALSE, n.pattern = NULL, digits = 2, as.na = NULL,
           plot = FALSE, geom = c("all", "square", "rotate"), filename = NULL,
           width = NA, height = NA,  dpi = 600, write = NULL, append = TRUE,
           check = TRUE, output = TRUE)
}
\arguments{
\item{data}{a data frame with incomplete data, where
missing values are coded as \code{NA}.}

\item{...}{an expression indicating the variable names in
\code{data} e.g., \code{na.pattern(dat, x1, x2, x3)}.
Note that the operators \code{+}, \code{-},
\code{~}, \code{:}, \code{::}, and \code{!} can also
be used to select variables, see 'Details' in the
\code{\link{df.subset}} function.}

\item{order}{logical: if \code{TRUE}, variables are ordered from left to
right in increasing order of missing values.}

\item{n.pattern}{an integer value indicating the minimum number of cases sharing
a missing data pattern to be included in the result table and the plot, e.g., specifying
\code{n.pattern = 5} excludes missing data patters with less than \code{5} cases.}

\item{digits}{an integer value indicating the number of decimal places to
be used for displaying percentages.}

\item{as.na}{a numeric vector indicating user-defined missing values,
i.e. these values are converted to NA before conducting the
analysis.}

\item{plot}{logical: if \code{TRUE}, missing data pattern is plotted.}

\item{geom}{a character vector indicating which plot elements to
draw, i.e.,  \code{"all"} for drawing all plot elements,
\code{"square"} the plot tiles are squares to mimic
the \code{md.pattern} function in the package \pkg{mice},
and \code{"rotate"} for rotating variable name labels
90 degrees.}

\item{filename}{a character string indicating the \code{filename} argument (default is \code{"NA_Pattern.pdf"}) including
the file extension for the \code{ggsave} function. Note that one of \code{".eps"}, \code{".ps"},
\code{".tex"}, \code{".pdf"} (default), \code{".jpeg"}, \code{".tiff"}, \code{".png"}, \code{".bmp"},
\code{".svg"} or \code{".wmf"} needs to be specified as file extension in the \code{file} argument.}

\item{width}{a numeric value indicating the \code{width} argument (default is the
size of the current graphics device) for the \code{ggsave} function.}

\item{height}{a numeric value indicating the \code{height} argument
(default is the size of the current graphics device) for the \code{ggsave} function.}

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

\item{output}{logical: if \code{TRUE} (default), output is shown.}
}
\value{
Returns an object of class \code{misty.object}, which is a list with following
entries:
\item{\code{call}}{function call}
\item{\code{type}}{type of analysis}
\item{\code{data}}{data frame with variables used in the analysis}
\item{\code{args}}{specification of function arguments}
\item{\code{result}}{result table}
\item{\code{plot}}{ggplot2 object for plotting the results}
\item{\code{pattern}}{a numeric vector indicating the missing data pattern for each case}
}
\description{
This function computes a summary of missing data patterns, i.e., number (%) of
cases with a specific missing data pattern and plots the missing data
patterns.
}
\note{
The code for plotting missing data patterns is based on the \code{plot_pattern}
function in the \pkg{ggmice} package by Hanne Oberman.
}
\examples{
\dontrun{
#————————————————————————————————————————————————————————————————————————————
# Missing Data Pattern

# Example 1a: Compute a summary of missing data patterns
dat.pattern <- na.pattern(airquality)

# Example 1b: Exclude missing data patterns with less than 3 cases
na.pattern(airquality, plot = TRUE, n.pattern = 3)

# Example 1c: Vector of missing data pattern for each case
dat.pattern$pattern

# Example 1d: Data frame without cases with missing data pattern 2
airquality[dat.pattern$pattern != 2, ]

#————————————————————————————————————————————————————————————————————————————
# Plot

# Example 2a: Plot a summary of missing data patterns
na.pattern(airquality, plot = TRUE)

# Example 2b: Plot results using the plot() function, use additional arguments
# see Details in the help page of the function plot.misty.object
object <- na.pattern(airquality)
plot(object, tile.alpha = 0.8, scale.fill.col = c("red2", "darkgreen"))

#————————————————————————————————————————————————————————————————————————————
# Write Results

# Example 3a: Write Results into a text file
na.pattern(airquality, write = "NA_Pattern.xlsx")

# Example 3b: Write Results into a Excel file
na.pattern(airquality, write = "NA_Pattern.xlsx")

# Example 3c: Write Results into a text file
na.pattern(airquality, plot = TRUE, filename = "NA_Pattern.png", width = 8, height = 7)}
}
\references{
Enders, C. K. (2022). \emph{Applied missing data analysis} (2nd ed.). The Guilford Press.

Graham, J. W. (2009). Missing data analysis: Making it work in the real world.
\emph{Annual Review of Psychology, 60}, 549-576.
https://doi.org/10.1146/annurev.psych.58.110405.085530

Oberman, H. (2023). \emph{ggmice: Visualizations for 'mice' with 'ggplot2'}.
R package version 0.1.0. https://doi.org/10.32614/CRAN.package.ggmice

van Buuren, S. (2018). \emph{Flexible imputation of missing data} (2nd ed.).
Chapman & Hall.
}
\seealso{
\code{\link{write.result}}, \code{\link{as.na}}, \code{\link{na.as}},
\code{\link{na.auxiliary}}, \code{\link{na.coverage}}, \code{\link{na.descript}},
\code{\link{na.indicator}}, \code{\link{na.prop}}, \code{\link{na.test}}
}
\author{
Takuya Yanagida
}
