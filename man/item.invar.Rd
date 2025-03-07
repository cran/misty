% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/item.invar.R
\name{item.invar}
\alias{item.invar}
\title{Between-Group and Longitudinal Measurement Invariance Evaluation}
\usage{
item.invar(..., data = NULL, model = NULL, rescov = NULL, rescov.long = TRUE,
           group = NULL, long = FALSE, cluster = NULL,
           invar = c("config", "metric", "scalar", "strict"),
           partial = NULL, ident = c("marker", "var", "effect"),
           estimator = c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR",
                         "GLS", "WLS", "DWLS", "WLSM", "WLSMV",
                         "ULS", "ULSM", "ULSMV", "DLS", "PML"),
           missing = c("listwise", "pairwise", "fiml", "two.stage",
                       "robust.two.stage", "doubly.robust"), null.model = TRUE,
           print = c("all", "summary", "coverage", "descript", "fit", "est",
                     "modind", "resid"),
           print.fit = c("all", "standard", "scaled", "robust"),
           mod.minval = 6.63, resid.minval = 0.1, digits = 3, p.digits = 3,
           as.na = NULL, write = NULL, append = TRUE, check = TRUE, output = TRUE)
}
\arguments{
\item{...}{a matrix or data frame. If \code{model = NULL}, confirmatory
factor analysis based on a measurement model with one factor
labeled \code{f} comprising all variables in the matrix or
data frame specified in \code{x} for evaluating between-group
measurement invariance for the grouping variable specified
in the argument \code{group} is conducted. Longitudinal
measurement invariance evaluation can only be conducted by
specifying the model using the argument \code{model}. Note
that the cluster variable is excluded from \code{x} when
specifying \code{cluster}. If \code{model} is specified,
the matrix or data frame needs to contain all variables
used in the argument \code{model} and the cluster variable
when specifying the name of the cluster variable in the
argument \code{cluster}. Alternatively, an expression
indicating the variable names in \code{data} e.g.,
\code{item.invar(x1, x2, x2, data = dat)}. Note that the operators
\code{.}, \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
and \code{!} can also be used to select variables, see 'Details'
in the \code{\link{df.subset}} function.}

\item{data}{a data frame when specifying one or more variables in the
argument \code{...}. Note that the argument is \code{NULL}
when specifying a vector, factor, matrix, array, data frame,
or list for the argument \code{...}.}

\item{model}{a character vector specifying a measurement model with one
factor, or a list of character vectors for specifying a
measurement model with more than one factor for evaluating
between-group measurement invariance when \code{long = FALSE}
or a list of character vectors for specifying a measurement
model with one factor for each time of measurement for
evaluating longitudinal measurement invariance when
specifying \code{long = TRUE}. For example,
\code{model = c("x1", "x2", "x3", "x4")} for specifying a
measurement model with one factor labeled \code{f} comprising
four indicators, or \code{model = list(factor1 = c("x1", "x2", "x3", "x4"),
factor2 = c("x5", "x6", "x7", "x8"))} for specifying a
measurement model with two latent factors labeled \code{factor1}
and \code{factor2} each comprising four indicators for
evaluating between-group measurement invariance, or
\code{model = list(time1 = c("ax1", "ax2", "ax3", "ax4"),
time2 = c("bx1", "bx2", "bx3", "bx4"),
time3 = c("cx1", "cx2", "cx3", "cx4"))} for specifying a
longitudinal measurement model with three time points comprising
four indicators at each time point. This function cannot
evaluate longitudinal measurement invariance for a measurement
model with more than one factor. Note that the name of each
list element is used to label factors, i.e., all list elements
need to be named, otherwise factors are labeled with \code{"f1", "f2", "f3"}
when \code{long = FALSE} and with \code{"t1", "t2", "t3"}
when \code{long = TRUE} and so on.}

\item{rescov}{a character vector or a list of character vectors for specifying
residual covariances, e.g., \code{rescov = c("x1", "x2")}
for specifying a residual covariance between items \code{x1}
and \code{x2}, or \code{rescov = list(c("x1", "x2"), c("x3", "x4"))}
for specifying residual covariances between items \code{x1}
and \code{x2}, and items \code{x3} and \code{x4}.}

\item{rescov.long}{logical: if \code{TRUE} (default), residual covariances between
parallel indicators are estimated across time when evaluating
longitudinal measurement invariance (\code{long = TRUE}),
i.e., residual variances of the same indicators that are
measured at different time points are correlated across all
possible time points. Note that residual covariances should
be estimated even if the parameter estimates are statistically
not significant since indicator-specific systematic variance
is likely to correlate with itself over time (Little, 2013,
p. 164).}

\item{group}{either a character string indicating the variable name of
the grouping variable in the matrix or data frame specified
in \code{x} or a vector representing the groups
for conducting multiple-group analysis to evaluate between-group
measurement invariance.}

\item{long}{logical: if \code{TRUE}, longitudinal measurement invariance
evaluation is conducted. The longitudinal measurement model
is specified by using the argument \code{model}. Note that
this function can only evaluate either between-group or
longitudinal measurement invariance, but not both at the
same time.}

\item{cluster}{either a character string indicating the variable name
of the cluster variable in \code{...} or \code{data},
or a vector representing the nested grouping structure
(i.e., group or cluster variable) for computing
cluster-robust standard errors. Note that cluster-robust
standard errors are not available when treating indicators
of the measurement model as ordered (ordinal).}

\item{invar}{a character string indicating the level of measurement
invariance to be evaluated, i.e., \code{config} to evaluate
configural measurement invariance (i.e., same factor structure
across groups or time), \code{metric} to evaluate configural
and metric measurement invariance (i.e., equal factor loadings
across groups or time), \code{scalar} (default) to evaluate
configural, metric and scalar measurement invariance (i.e.,
equal intercepts or thresholds across groups or time), and
\code{strict} to evaluate configural, metric, scalar, and
strict measurement invariance (i.e., equal residual variances
across groups or time).}

\item{partial}{a character string or character vector containing the labels
of the parameters which should be free in all groups or across
time to specify a partial measurement invariance model. Note
that the labels of the parameters need to match the labels
shown in the output, i.e., \code{"L"} with a number for factor
loadings, \code{"T"} with a number for intercepts, and
\code{"E"} with a number for factor residual variances. The
number attached to the \code{"L"}, \code{"T"}, or \code{"E"}
label corresponds to the number of the indicator in the
measurement model (e.g., \code{"T3"} for the intercept of
the third indicator). When specifying the model using the
argument \code{model}, however, the number for the factor
loading is a combination of the number of the factor and
the number of the indicator (e.g., \code{"L23"} is the third
indicator of the second factor). Note that at least two
invariant indicators are needed for a partial measurement
invariance model. Otherwise there might be issues with model
non-identification.}

\item{ident}{a character string indicating the method used for identifying
and scaling latent variables, i.e., \code{"marker"} for the
marker variable method fixing the first factor loading of
each latent variable to 1, \code{"var"} (default) for the
fixed variance method fixing the variance of each latent
variable to 1, or \code{"effect"} for the effects-coding
method using equality constraints so that the average of
the factor loading for each latent variable equals 1.}

\item{estimator}{a character string indicating the estimator to be used
(see 'Details' in the help page of the \code{item.cfa()}
function). By default, \code{"MLR"} is used for CFA models
with continuous indicators.}

\item{missing}{a character string indicating how to deal with missing data,
i.e., \code{"listwise"} for listwise deletion, \code{"pairwise"}
for pairwise deletion, \code{"fiml"} for full information
maximum likelihood method, \code{two.stage} for two-stage
maximum likelihood method, \code{robust.two.stage} for robust
two-stage maximum likelihood method, and \code{doubly-robust}
for doubly-robust method (see 'Details' in the help page
of the\code{item.cfa()} function). By default, \code{"fiml"}
is used for CFA models with continuous indicators which are
estimated by using \code{estimator = "MLR"}. However, argument
\code{missing} switches to \code{listwise} when the data
set is complete, i.e., it is not possible to use FIML in
complete data. Note that the robust CFI, TLI, and RMSEA
are different in complete data depending on whether FIML or
listwise deletion was specified when estimating the model
in lavaan.}

\item{null.model}{logical: if \code{TRUE} (default), the proper null model
for computing incremental fit indices (i.e., CFI and TLI)
is used, i.e., means and variances of the indicators are
constrained to be equal across group or time in the null
model (Little, 2013, p. 112).}

\item{print}{a character string or character vector indicating which results
to show on the console, i.e. \code{"all"} for all results,
\code{"summary"} for a summary of the specification of the
estimation method and missing data handling in lavaan,
\code{"coverage"} for the variance-covariance coverage of
the data, \code{"descript"} for descriptive statistics,
\code{"fit"} for model fit and model comparison, \code{"est"}
for parameter estimates, \code{"modind"} for modification
indices, and \code{"resid"} for the residual correlation
matrix and standardized residual means. By default, a summary
of the specification, model fit, and parameter estimates
are printed. Note that parameter estimates, modification
indices, and residual correlation matrix is only provided
for the model investigating the level of measurement
invariance specified in the argument \code{"invar"}.}

\item{print.fit}{a character string or character vector indicating which
version of the CFI, TLI, and RMSEA to show on the console
when using a robust estimation method involving a scaling
correction factor, i.e., \code{"all"} for all versions of
the CFI, TLI, and RMSEA, \code{"standard"} (default when
\code{estimator} is one of \code{"ML", "MLF", "GLS", "WLS", "DWLS", "ULS", "PML"})
for fit indices without any non-normality correction,
\code{"scaled"} for population-corrected robust fit indices
with ad hoc non-normality correction, and \code{robust}
(default when \code{estimator} is one of \code{"MLM", "MLMV", "MLMVS", "MLR", "WLSM", "WLSMV", "ULSM", "ULSMV", "DLS"})
for sample-corrected robust fit indices based on formula
provided by Li and Bentler (2006) and Brosseau-Liard and
Savalei (2014).}

\item{mod.minval}{numeric value to filter modification indices and only show
modifications with a modification index value equal or higher
than this minimum value. By default, modification indices
equal or higher 6.63 are printed. Note that a modification
index value of 6.63 is equivalent to a significance level
of \eqn{\alpha = .01}.}

\item{resid.minval}{numeric value indicating the minimum absolute residual
correlation coefficients and standardized means to highlight
in boldface. By default, absolute residual correlation
coefficients and standardized means equal or higher 0.1
are highlighted. Note that highlighting can be disabled by
setting the minimum value to 1.}

\item{digits}{an integer value indicating the number of decimal places
to be used for displaying results. Note that information
criteria and chi-square test statistic are printed with
\code{digits} minus 1 decimal places.}

\item{p.digits}{an integer value indicating the number of decimal places
to be used for displaying \emph{p}-values, covariance coverage
(i.e., \code{p.digits - 1}), and residual correlation
coefficients.}

\item{as.na}{a numeric vector indicating user-defined missing values, i.e.,
these values are converted to \code{NA} before conducting
the analysis. Note that \code{as.na()} function is only
applied to \code{x} but not to \code{group} or \code{cluster}.}

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

\item{check}{logical: if \code{TRUE} (default), argument specification is checked
and convergence and model identification checks are conducted
for all estimated models.}

\item{output}{logical: if \code{TRUE} (default), output is shown.}
}
\value{
Returns an object of class \code{misty.object}, which is a list with following
entries:

\item{\code{call}}{function call}
\item{\code{type}}{type of analysis}
\item{\code{data}}{data frame including all variables used in the analysis, i.e.,
                   indicators for the factor, grouping variable and cluster variable}
\item{\code{args}}{specification of function arguments}
\item{\code{model}}{list with specified model for the configural, metric, scalar,
                    and strict invariance model}
\item{\code{model.fit}}{list with fitted lavaan object of the configural, metric,
                        scalar, and strict invariance model}
\item{\code{check}}{list with the results of the convergence and model identification
                    check for the configural, metric, scalar, and strict invariance
                    model}
\item{\code{result}}{list with result tables, i.e., \code{summary} for the
                     summary of the specification of the estimation method and
                     missing data handling in lavaan, \code{coverage} for the
                     variance-covariance coverage of the data, \code{descript}
                     for descriptive statistics, \code{fit} for a list with
                     model fit based on standard, scaled, and robust fit indices,
                     \code{est} for a list with parameter estimates for the
                     configural, metric, scalar, and strict invariance model,
                     \code{modind} for the list with modification indices for
                     the configural, metric, scalar, and strict invariance model,
                     \code{score} for the list with result of the score tests
                     for constrained parameters for the configural, metric,
                     scalar, and strict invariance model, and \code{resid} for
                     the list with residual correlation matrices and standardized
                     residual means for the configural, metric, scalar, and
                     strict invariance model}
}
\description{
This function is a wrapper function for evaluating configural, metric, scalar,
and strict between-group or longitudinal (partial) measurement invariance using
confirmatory  factor analysis with continuous indicators by calling the \code{cfa}
function in the R package \pkg{lavaan}. By default, the function evaluates
configural, metric, and scalar measurement invariance by providing a table
with model fit information (i.e., chi-square test, fit indices based on a proper
null model, and information criteria) and model comparison (i.e., chi-square
difference test, change in fit indices, and change in information criteria).
Additionally, variance-covariance coverage of the data, descriptive statistics,
parameter estimates, modification indices, and residual correlation matrix can
be requested by specifying the argument \code{print}.
}
\note{
The function uses the functions \code{cfa}, \code{fitmeasures} ,\code{lavInspect},
\code{lavTech}, \code{lavTestLRT}, \code{lavTestScore}, \code{modindices},
\code{parameterEstimates}, \code{parTable}, and \code{standardizedsolution}
provided in the R package \pkg{lavaan} by Yves Rosseel (2012).
}
\examples{
\dontrun{

# Load data set "HolzingerSwineford1939" in the lavaan package
data("HolzingerSwineford1939", package = "lavaan")

#-------------------------------------------------------------------------------
# Between-Group Measurement Invariance Evaluation

#..................
# Measurement model with one factor

# Example 1a: Specification of the grouping variable in 'x'
item.invar(HolzingerSwineford1939[, c("x1", "x2", "x3", "x4", "sex")], group = "sex")

# Example 1b: Specification of the grouping variable in 'group'
item.invar(HolzingerSwineford1939[, c("x1", "x2", "x3", "x4")],
           group = HolzingerSwineford1939$sex)

# Example 1c: Alternative specification using the 'data' argument
item.invar(x1:x4, data = HolzingerSwineford1939, group = "sex")

# Example 1d: Alternative specification using the argument 'model'
item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"), group = "sex")

# Example 1e: Alternative specification using the 'data' and 'model' argument
item.invar(., data = HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"), group = "sex")

#..................
# Measurement model with two factors

item.invar(HolzingerSwineford1939,
           model = list(c("x1", "x2", "x3", "x4"),
                        c("x5", "x6", "x7", "x8")), group = "sex")

#..................
# Configural, metric, scalar, and strict measurement invariance

# Example 2: Evaluate configural, metric, scalar, and strict measurement invariance
item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
           group = "sex", invar = "strict")

#..................
# Partial measurement invariance

# Example 3: Free second factor loading (L2) and third intercept (T3)
item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
           group = "sex", partial = c("L2", "T3"), print = c("fit", "est"))

#..................
# Residual covariances

# Example 4a: One residual covariance
item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
           rescov = c("x3", "x4"), group = "sex")

# Example 4b: Two residual covariances
item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
           rescov = list(c("x1", "x2"), c("x3", "x4")), group = "sex")

#..................
# Scaled test statistic and cluster-robust standard errors

# Example 5a: Specify cluster variable using a variable name in 'x'
item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
           group = "sex", cluster = "agemo")

# Example 5b: Specify vector of the cluster variable in the argument 'cluster'
item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
           group = "sex", cluster = HolzingerSwineford1939$agemo)

#..................
# Default Null model

# Example 6: Specify default null model for computing incremental fit indices
item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
           group = "sex", null.model = FALSE)

#..................
# Print argument

# Example 7a: Request all results
item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
           group = "sex", print = "all")

# Example 7b: Request fit indices with ad hoc non-normality correction
item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
           group = "sex", print.fit = "scaled")

# Example 7c: Request modification indices with value equal or higher than 10
# and highlight residual correlations equal or higher than 0.3
item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
           group = "sex", print = c("modind", "resid"),
           mod.minval = 10, resid.minval = 0.3)

#..................
# Model syntax and lavaan summary of the estimated model

# Example 8
mod <- item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
                  group = "sex", output = FALSE)

# lavaan model syntax scalar invariance model
cat(mod$model$scalar)

# lavaan summary of the scalar invariance model
lavaan::summary(mod$model.fit$scalar, standardized = TRUE, fit.measures = TRUE)

#-------------------------------------------------------------------------------
# Longitudinal Measurement Invariance Evaluation

# Example 9: Two time points with three indicators at each time point
item.invar(HolzingerSwineford1939,
           model = list(c("x1", "x2", "x3"),
                        c("x5", "x6", "x7")), long = TRUE)

#------------------------------------------------
# Write Results

# Example 10a: Write results into a text file
item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
            group = "sex", print = "all", write = "Invariance.txt", output = FALSE)

# Example 10b: Write results into an Excel file
item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
            group = "sex", print = "all", write = "Invariance.xlsx", output = FALSE)

result <- item.invar(HolzingerSwineford1939, model = c("x1", "x2", "x3", "x4"),
                     group = "sex", print = "all", output = FALSE)
write.result(result, "Invariance.xlsx")
}
}
\references{
Brosseau-Liard, P. E., & Savalei, V. (2014) Adjusting incremental fit indices
for nonnormality. \emph{Multivariate Behavioral Research, 49}, 460-470.
https://doi.org/10.1080/00273171.2014.933697

Li, L., & Bentler, P. M. (2006). Robust statistical tests for evaluating the
hypothesis of close fit of misspecified mean and covariance structural models.
\emph{UCLA Statistics Preprint #506}. University of California.

Little, T. D. (2013). \emph{Longitudinal structural equation modeling}. Guilford
Press.

Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling.
\emph{Journal of Statistical Software, 48}, 1-36. https://doi.org/10.18637/jss.v048.i02
}
\seealso{
\code{\link{item.cfa}}, \code{\link{multilevel.invar}}, \code{\link{write.result}}
}
\author{
Takuya Yanagida \email{takuya.yanagida@univie.ac.at}
}
