% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rwg.lindell.R
\name{rwg.lindell}
\alias{rwg.lindell}
\title{Lindell, Brandt and Whitney (1999) r*wg(j) Within-Group Agreement Index for Multi-Item Scales}
\usage{
rwg.lindell(x, group, A = NULL, ranvar = NULL, z = TRUE, expand = TRUE, na.omit = FALSE,
            as.na = NULL, check = TRUE)
}
\arguments{
\item{x}{a matrix or data frame with numeric vectors.}

\item{group}{a vector representing the grouping structure (i.e., group variable).}

\item{A}{a numeric value indicating the number of discrete response options of the items
from which the random variance is computed based on \eqn{(A^2 - 1) / 12}. Note that
either the argument \code{j} or the argument\code{ranvar} is specified.}

\item{ranvar}{a numeric value indicating the random variance to which the mean of the item variance
is divided. Note that either the argument \code{j} or the argument\code{ranvar} is
specified.}

\item{z}{logical: if \code{TRUE}, Fisher z-transformation based on the formula
\eqn{z = 0.5*log((1 + r) / (1 - r))} is applied to the vector of r*wg(j) estimates.}

\item{expand}{logical: if \code{TRUE}, vector of r*wg(j) estimates is expanded to match the input
vector \code{x}.}

\item{na.omit}{logical: if \code{TRUE}, incomplete cases are removed before conducting the analysis
(i.e., listwise deletion).}

\item{as.na}{a numeric vector indicating user-defined missing values,
i.e. these values are converted to \code{NA} before conducting the analysis.
Note that \code{as.na()} function is only applied to \code{x}, but not to \code{group}.}

\item{check}{logical: if \code{TRUE}, argument specification is checked.}
}
\value{
Returns a numeric vector containing r*wg(j) agreement index for multi-item scales with the same
length as \code{group} if \code{expand = TRUE} or a data frame with following entries if
\code{expand = FALSE}:

\tabular{ll}{
  \code{group}            \tab group identifier \cr
  \code{n}                \tab group size \code{x} \cr
  \code{rwg.lindell}      \tab r*wg(j) estimate for each group \cr
  \code{z.rwg.lindell}    \tab Fisher z-transformed r*wg(j) estimate for each group \cr
}
}
\description{
This function computes r*wg(j) within-group agreement index for multi-item scales as described in
Lindell, Brandt and Whitney (1999).
}
\details{
The r*wg(j) index is calculated by dividing the mean of the item variance by the expected random
variance (i.e., null distribution). The default null distribution in most research is the rectangular
or uniform distribution calculated with \eqn{\sigma^2_eu = (A^2 - 1) / 12}, where \eqn{A} is the number
of discrete response options of the items. However, what constitutes a reasonable standard for random
variance is highly debated. Note that the r*wg(j) allows that the mean of the item variances to be
larger than the expected random variances, i.e., r*wg(j) values can be negative.

Note that the \code{rwg.j.lindell()} function in the \pkg{multilevel} package uses listwise deletion
by default, while the \code{rwg.lindell()} function uses all available information to compute the
r*wg(j) agreement index by default. In order to obtain equivalent results in the presence of missing
values, listwise deletion (\code{na.omit = TRUE}) needs to be applied.

Examples for the application of r*wg(j) within-group agreement index for multi-item scales can be
found in Bardach, Yanagida, Schober and Lueftenegger (2018), Bardach, Lueftenegger, Yanagida, Schober
and Spiel (2018), and Bardach, Lueftenegger, Yanagida, Spiel and Schober (2019).
}
\examples{
dat <- data.frame(id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                  group = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
                  x1 = c(2, 3, 2, 1, 1, 2, 4, 3, 5),
                  x2 = c(3, 2, 2, 1, 2, 1, 3, 2, 5),
                  x3 = c(3, 1, 1, 2, 3, 3, 5, 5, 4))

# Compute Fisher z-transformed r*wg(j) for a multi-item scale with A = 5 response options
rwg.lindell(dat[, c("x1", "x2", "x3")], group = dat$group, A = 5)

# Compute Fisher z-transformed r*wg(j) for a multi-item scale with a random variance of 2
rwg.lindell(dat[, c("x1", "x2", "x3")], group = dat$group, ranvar = 2)

# Compute r*wg(j) for a multi-item scale with A = 5 response options
rwg.lindell(dat[, c("x1", "x2", "x3")], group = dat$group, A = 5, z = FALSE)

# Compute Fisher z-transformed r*wg(j) for a multi-item scale with A = 5 response options,
# do not expand the vector
rwg.lindell(dat[, c("x1", "x2", "x3")], group = dat$group, A = 5, expand = FALSE)
}
\references{
Bardach, L., Lueftenegger, M., Yanagida, T., & Schober, B. (2019). Achievement or agreement - Which comes first?
Clarifying the temporal ordering of achievement and within-class consensus on classroom goal structures.
\emph{Learning and Instruction, 61}, 72-83. https://doi.org/10.1016/j.learninstruc.2019.01.003

Bardach, L., Lueftenegger, M., Yanagida, T., Schober, B. & Spiel, C. (2019). The role of within-class consensus
on mastery goal structures in predicting socio-emotional outcomes. \emph{British Journal of Educational Psychology, 89},
239-258. https://doi.org/10.1111/bjep.12237

Bardach, L., Yanagida, T., Schober, B. & Lueftenegger, M. (2018). Within-class consensus on classroom goal structures:
Relations to achievement and achievement goals in mathematics and language classes. \emph{Learning and Individual Differences, 67},
78-90. https://doi.org/10.1016/j.lindif.2018.07.002

Lindell, M. K., Brandt, C. J., & Whitney, D. J. (1999). A revised index of interrater
agreement for multi-item ratings of a single target. \emph{Applied Psychological Measurement}, \emph{23},
127-135. https://doi.org/10.1177/01466219922031257

O'Neill, T. A. (2017). An overview of interrater agreement on Likert scales for researchers and practitioners.
\emph{Frontiers in Psychology}, \emph{8}, Article 777. https://doi.org/10.3389/fpsyg.2017.00777
}
\seealso{
\code{\link{group.scores}}
}
\author{
Takuya Yanagida \email{takuya.yanagida@univie.ac.at}
}
