% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcnemartestClust.R
\name{mcnemartestClust}
\alias{mcnemartestClust}
\title{Test of Marginal Homogeneity for Clustered Data}
\usage{
mcnemartestClust(x, y, id, variance = c("MoM", "emp"))
}
\arguments{
\item{x, y}{vector or factor objects of equal length.}

\item{id}{a vector or factor object which identifies the clusters. The length of \code{id} must be the same
as the length of \code{x}.}

\item{variance}{character string specifying the method of variance estimation. Must be one of "\code{MoM}"
or "\code{emp}".}
}
\value{
A list with class "\code{htest}" containing the following components:
\item{statistic}{the value of the test statistic.}
\item{parameter}{the degrees of freedom of the approximate chi-squared distribution of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{method}{a character string indicating the test performed and which variance estimation method was used.}
\item{data.name}{a character string giving the name(s) of the data and the total number of clusters.}
\item{M}{the number of clusters.}
}
\description{
Performs a test of marginal homogeneity of paired clustered data with potentially informative
cluster size.
}
\details{
The null is that the marginal probabilities of being classified into cells \code{[i,j]} and
\code{[j,i]} are equal.

Arguments \code{x}, \code{y}, and \code{id} must be vectors or factors of the same length.
Incomplete cases are removed.

When \code{variance} is \code{MoM}, a method of moments variance estimate evaluated under the null is used.
This is equivalent to the test by Durkalski \emph{et al.} (2003). When \code{variance} is \code{emp},
an empirical variance estimate is used. See Gregg (2020) for details.
}
\examples{
data(screen8)
## Is marginal proportion of students in lowest fitness category
## at the end of year equal to the beginning of year?
screen8$low.start <- 1*(screen8$qfit.s=='Q1')
screen8$low.end <- 1*(screen8$qfit=='Q1')
mcnemartestClust(screen8$low.start, screen8$low.end, screen8$sch.id)

}
\references{
Durkalski, V., Palesch, Y., Lipsitz, S., Rust, P. (2003). Analysis of clustered matched pair data.
\emph{Statistics in Medicine}, \bold{22}, 2417--2428.

Gregg, M., Marginal methods and software for clustered data with cluster- and group-size informativeness.
PhD dissertation, University of Louisville, 2020.
}
