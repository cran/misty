% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster.scores.R
\name{cluster.scores}
\alias{cluster.scores}
\title{Cluster Scores}
\usage{
cluster.scores(x, cluster, fun = c("mean", "sum", "median", "var", "sd", "min", "max"),
               expand = TRUE, as.na = NULL, check = TRUE)
}
\arguments{
\item{x}{a numeric vector.}

\item{cluster}{a vector representing the nested grouping structure (i.e., group or
cluster variable).}

\item{fun}{character string indicating the function used to compute cluster scores, default: \code{"mean"}.}

\item{expand}{logical: if \code{TRUE}, vector of cluster scores is expanded to match the input vector \code{x}.}

\item{as.na}{a numeric vector indicating user-defined missing values,
i.e. these values are converted to \code{NA} before conducting the analysis.
Note that \code{as.na()} function is only applied to the argument \code{x},
but not to \code{cluster}.}

\item{check}{logical: if \code{TRUE}, argument specification is checked.}
}
\value{
Returns a numeric vector containing cluster scores with the same length as \code{x} if \code{expand = TRUE}
or with the length \code{length(unique(cluster))} if \code{expand = FALSE}.
}
\description{
This function computes cluster means by default.
}
\examples{
dat.ml <- data.frame(id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                     cluster = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
                     x = c(4, 2, 5, 6, 3, 4, 1, 3, 4))

# Compute cluster means and expand to match the input x
cluster.scores(dat.ml$x, cluster = dat.ml$cluster)

# Compute standard deviation for each cluster and expand to match the input x
cluster.scores(dat.ml$x, cluster = dat.ml$cluster, fun = "sd")

# Compute cluster means without expanding the vector
cluster.scores(dat.ml$x, cluster = dat.ml$cluster, expand = FALSE)
}
\references{
Hox, J., Moerbeek, M., & van de Schoot, R. (2018). \emph{Multilevel analysis: Techniques and applications} (3rd. ed.).
Routledge.

Snijders, T. A. B., & Bosker, R. J. (2012). \emph{Multilevel analysis: An introduction to basic and advanced multilevel
modeling} (2nd ed.). Sage Publishers.
}
\seealso{
\code{\link{item.scores}}, \code{\link{multilevel.descript}}, \code{\link{multilevel.icc}}
}
\author{
Takuya Yanagida \email{takuya.yanagida@univie.ac.at}
}