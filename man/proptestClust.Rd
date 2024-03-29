% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proptestClust.R
\name{proptestClust}
\alias{proptestClust}
\title{Test of Marginal Proportion for Clustered Data}
\usage{
proptestClust(x, id, p = NULL, alternative = c("two.sided", "less", "greater"),
             variance = c("sand.null", "sand.est", "emp", "MoM"), conf.level = 0.95)
}
\arguments{
\item{x}{a vector of binary indicators denoting success/failure of each observation, or a two-dimensional table
(or matrix) with 2 columns giving the aggregate counts of failures and successes (respectively) across clusters.}

\item{id}{a vector which identifies the clusters; ignored if \code{x} is a matrix or table.
The length of \code{id} must be the same as the length of \code{x}.}

\item{p}{the null hypothesized value of the marginal proportion. Must be a single number greater than 0 and less
than 1.}

\item{alternative}{a character string specifying the alternative hypothesis. Must be one of "\code{two.sided}",
"\code{greater}", or "\code{less}". You can specify just the initial letter.}

\item{variance}{character string specifying the method of variance estimation. Must be one of "\code{sand.null}",
"\code{sand.est}", "\code{emp}", or "\code{MoM}".}

\item{conf.level}{confidence level of the returned confidence interval. Must be a single number between
0 and 1.}
}
\value{
A list with class "\code{htest}" containing the following components:
\item{statistic}{the value of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{estimate}{the estimated marginal proportion.}
\item{null.value}{the value of \code{p} under the null hypothesis.}
\item{conf.int}{a confidence interval for the true marginal proportion.}
\item{alternative}{a character string describing the alternative hypothesis.}
\item{method}{a character string indicating the test performed and method of variance estimation.}
\item{data.name}{a character string giving the name of the data and the total number of clusters.}
\item{M}{the number of clusters.}
}
\description{
\code{proptestClust} can be used for testing the null that the marginal proportion
(probability of success) is equal to certain given values in clustered data with potentially informative
cluster size.
}
\details{
If \code{p} is not given, the null tested is that the underlying marginal probability of
success is .5.

The \code{variance} argument allows the user to specify the method of variance estimation, selecting from
the sandwich estimate evaluated at the null hypothesis (\code{sand.null}), the sandwich estimate evaluated at the
cluster-weighted proportion (\code{sand.est}), the empirical estimate (\code{emp}), or the method of moments
estimate (\code{MoM}).
}
\examples{
data(screen8)
## using vectors
## suppose math proficiency is determined by score >= 65
## is the marginal proportion of students proficient in math at least 75\%?
screen8$math.p <- 1*(screen8$math>=65)
proptestClust(screen8$math.p, screen8$sch.id, p = .75, alternative = "great")

## using table and empirical variance; two-sided CI
## (note that "failure" counts are the first column and "success" counts are the second column)
mathp.tab <- table(screen8$sch.id, screen8$math.p)
proptestClust(mathp.tab, variance="emp", p=.75)

## when all clusters have a size of 1, results will be in general correspondence with
## that of the classical analogue test
set.seed(123)
x <- rbinom(100, size = 1, p = 0.7)
id <- 1:100
proptestClust(x, id)
prop.test(sum(x), length(x))

}
\references{
Gregg, M., Datta, S., Lorenz, D. (2020) Variance Estimation in Tests of Clustered Categorical Data
with Informative Cluster Size.
\emph{Statistical Methods in Medical Research}, doi:10.1177/0962280220928572.
}
