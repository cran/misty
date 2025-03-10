% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df.merge.R
\name{df.merge}
\alias{df.merge}
\title{Merge Multiple Data Frames}
\usage{
df.merge(..., by, all = TRUE, check = TRUE, output = TRUE)
}
\arguments{
\item{...}{a sequence of matrices or data frames and/or matrices to be merged to one.}

\item{by}{a character string indicating the column used for merging (i.e., matching variable),
see 'Details'.}

\item{all}{logical: if \code{TRUE} (default), then extra rows with \code{NA}s will be added
to the output for each row in a data frame that has no matching row in
another data frame.}

\item{check}{logical: if \code{TRUE} (default), argument specification is checked.}

\item{output}{logical: if \code{TRUE} (default), output is shown on the console.}
}
\value{
Returns a merged data frame.
}
\description{
This function merges data frames by a common column (i.e., matching variable).
}
\details{
There are following requirements for merging multiple data frames: First, each data frame
has the same matching variable specified in the \code{by} argument. Second, matching variable
in the data frames have all the same class. Third, there are no duplicated values in the
matching variable in each data frame. Fourth, there are no missing values in the matching
variables. Last, there are no duplicated variable names across the data frames except for
the matching variable.

Note that it is possible to specify data frames matrices and/or in the argument \code{...}.
However, the function always returns a data frame.
}
\examples{
adat <- data.frame(id = c(1, 2, 3),
                   x1 = c(7, 3, 8))

bdat <- data.frame(id = c(1, 2),
                   x2 = c(5, 1))

cdat <- data.frame(id = c(2, 3),
                   y3 = c(7, 9))

ddat <- data.frame(id = 4,
                   y4 = 6)

# Example 1: Merge 'adat', 'bdat', 'cdat', and 'ddat' by the variable 'id'
df.merge(adat, bdat, cdat, ddat, by = "id")

# Example 2: Do not show output on the console
df.merge(adat, bdat, cdat, ddat, by = "id", output = FALSE)

\dontrun{
#-------------------------------------------------------------------------------
# Error messages

adat <- data.frame(id = c(1, 2, 3),
                   x1 = c(7, 3, 8))

bdat <- data.frame(code = c(1, 2, 3),
                   x2 = c(5, 1, 3))

cdat <- data.frame(id = factor(c(1, 2, 3)),
                   x3 = c(5, 1, 3))

ddat <- data.frame(id = c(1, 2, 2),
                   x2 = c(5, 1, 3))

edat <- data.frame(id = c(1, NA, 3),
                   x2 = c(5, 1, 3))

fdat <- data.frame(id = c(1, 2, 3),
                   x1 = c(5, 1, 3))

# Error 1: Data frames do not have the same matching variable specified in 'by'.
df.merge(adat, bdat, by = "id")

# Error 2: Matching variable in the data frames do not all have the same class.
df.merge(adat, cdat, by = "id")

# Error 3: There are duplicated values in the matching variable specified in 'by'.
df.merge(adat, ddat, by = "id")

# Error 4: There are missing values in the matching variable specified in 'by'.
df.merge(adat, edat, by = "id")

# Error 5: There are duplicated variable names across data frames.
df.merge(adat, fdat, by = "id")
}
}
\seealso{
\code{\link{df.duplicated}},
\code{\link{df.move}}, \code{\link{df.rbind}},
\code{\link{df.rename}}, \code{\link{df.sort}},
\code{\link{df.subset}}
}
\author{
Takuya Yanagida \email{takuya.yanagida@univie.ac.at}
}
