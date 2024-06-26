% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.mplus.R
\name{read.mplus}
\alias{read.mplus}
\title{Read Mplus Data File and Variable Names}
\usage{
read.mplus(file, sep = "", input = NULL, na = -99, print = FALSE, return.var = FALSE,
           encoding = "UTF-8-BOM", check = TRUE)
}
\arguments{
\item{file}{a character string indicating the name of the Mplus data
file with or without the file extension \code{.dat}, e.g.,
\code{"Mplus_Data.dat"} or \code{"Mplus_Data"}.
Note that it is not necessary to specify this argument when
\code{return.var = TRUE}.}

\item{sep}{a character string indicating the field separator (i.e.,
delimiter) used in the data file specified in \code{file}.
By default, the separator is 'white space', i.e., one or more
spaces, tabs, newlines or carriage returns.}

\item{input}{a character string indicating the Mplus input (\code{.inp})
or output file (\code{.out}) in which the variable names
are specified in the \code{VARIABLE:} section. Note that if
\code{input = NULL}, this function is equivalent to \code{read.table(file)}.}

\item{na}{a numeric vector indicating values to replace with \code{NA}.
By default, \code{-99} is replaced with \code{NA}. If
\code{-99} is not a missing value change the argument to
\code{NULL}.}

\item{print}{logical: if \code{TRUE}, variable names are printed on the
console.}

\item{return.var}{logical: if \code{TRUE}, the function returns the variable
names extracted from the Mplus input or output file only.}

\item{encoding}{character string declaring the encoding used on \code{file}
so the character data can be re-encoded.See the 'Encoding' section of the help
page for the \code{file} function, the 'R Data Import/Export Manual' and 'Note'.}

\item{check}{logical: if \code{TRUE} (default), argument specification is checked.}
}
\value{
A data frame containing a representation of the data in the file.
}
\description{
This function reads a Mplus data file and/or Mplus input/output file to return
a data frame with variable names extracted from the Mplus input/output file. Note
that by default \code{-99} in the Mplus data file is replaced with to \code{NA}.
}
\examples{
\dontrun{
# Example 1: Read Mplus data file and variable names extracted from the Mplus input file
dat <- read.mplus("Mplus_Data.dat", input = "Mplus_Input.inp")

# Example 2: Read Mplus data file and variable names extracted from the Mplus input file,
# print variable names on the console
dat <- read.mplus("Mplus_Data.dat", input = "Mplus_Input.inp", print = TRUE)

# Example 3: Read variable names extracted from the Mplus input file
varnames <- read.mplus(input = "Mplus_Input.inp", return.var = TRUE)
}
}
\references{
Muthen, L. K., & Muthen, B. O. (1998-2017). \emph{Mplus User's Guide} (8th ed.).
Muthen & Muthen.
}
\seealso{
\code{\link{read.dta}}, \code{\link{write.dta}}, \code{\link{read.sav}},
\code{\link{write.sav}}, \code{\link{read.xlsx}}, \code{\link{write.xlsx}}
}
\author{
Takuya Yanagida \email{takuya.yanagida@univie.ac.at}
}
