% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster.scores.R
\name{cluster.scores}
\alias{cluster.scores}
\title{Cluster Scores}
\usage{
cluster.scores(..., data = NULL, cluster,
              fun = c("mean", "sum", "median", "var", "sd", "min", "max"),
              expand = TRUE, append = TRUE, name = ".a", as.na = NULL,
              check = TRUE)
}
\arguments{
\item{...}{a numeric vector for computing cluster scores for a variable,
matrix or data frame for computing cluster scores for more than
one variable. Alternatively, an expression indicating the variable
names in \code{data} e.g., \code{ci.mean(x1, x2, data = dat)}.
Note that the operators
\code{.}, \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
and \code{!} can also be used to select variables, see 'Details'
in the \code{\link{df.subset}} function.}

\item{data}{a data frame when specifying one or more variables in the
argument \code{...}. Note that the argument is \code{NULL}
when specifying a numeric vector, matrix, or data frame for
the argument \code{...}.}

\item{cluster}{either a character string indicating the variable name of
the cluster variable in \code{...} or \code{data}, or a
vector representing the nested grouping structure (i.e.,
group or cluster variable).}

\item{fun}{character string indicating the function used to compute group
scores, default: \code{"mean"}.}

\item{expand}{logical: if \code{TRUE} (default), vector of cluster scores is expanded
to match the input vector \code{x}.}

\item{append}{logical: if \code{TRUE} (default), cluster scores are
appended to the data frame specified in the argument \code{data}.}

\item{name}{a character string or character vector indicating the names
of the computed variables. By default, variables are named with the ending
\code{".a"} resulting in e.g. \code{"x1.a"} and \code{"x2.a"}. Variable names
can also be specified using a character vector matching the number
of variables specified in \code{x} (e.g.,
\code{name = c("cluster.x1", "cluster.x2")}).}

\item{as.na}{a numeric vector indicating user-defined missing values, i.e.
these values are converted to \code{NA} before conducting the
analysis. Note that \code{as.na()} function is only applied to
the argument \code{x}, but not to \code{cluster}.}

\item{check}{logical: if \code{TRUE} (default), argument specification is checked.}
}
\value{
Returns a numeric vector or data frame containing cluster scores with the same
length or same number of rows as \code{x} if \code{expand = TRUE} or with the
length or number of rows as \code{length(unique(cluster))} if \code{expand = FALSE}.
}
\description{
This function computes group means by default.
}
\examples{
# Load data set "Demo.twolevel" in the lavaan package
data("Demo.twolevel", package = "lavaan")

# Example 1a: Compute cluster means for 'y1' and expand to match the input 'y1'
cluster.scores(Demo.twolevel$y1, cluster = Demo.twolevel$cluster)

# Example 1b: Alternative specification using the 'data' argument
cluster.scores(y1, data = Demo.twolevel, cluster = "cluster")

# Example 2: Compute standard deviation for each cluster
# and expand to match the input x
cluster.scores(Demo.twolevel$y1, cluster = Demo.twolevel$cluster, fun = "sd")

# Example 3: Compute cluster means without expanding the vector
cluster.scores(Demo.twolevel$y1, cluster = Demo.twolevel$cluster, expand = FALSE)

# Example 4a: Compute cluster means for 'y1' and 'y2' and append to 'Demo.twolevel'
cbind(Demo.twolevel,
      cluster.scores(Demo.twolevel[, c("y1", "y2")], cluster = Demo.twolevel$cluster))

# Example 4b: Alternative specification using the 'data' argument
cluster.scores(y1, y2, data = Demo.twolevel, cluster = "cluster")
}
\references{
Hox, J., Moerbeek, M., & van de Schoot, R. (2018). \emph{Multilevel analysis:
Techniques and applications} (3rd. ed.). Routledge.

Snijders, T. A. B., & Bosker, R. J. (2012). \emph{Multilevel analysis: An
introduction to basic and advanced multilevel modeling} (2nd ed.). Sage
Publishers.
}
\seealso{
\code{\link{item.scores}}, \code{\link{multilevel.descript}},
\code{\link{multilevel.icc}}
}
\author{
Takuya Yanagida \email{takuya.yanagida@univie.ac.at}
}
