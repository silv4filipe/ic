% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit1.R
\name{mle1.GTDL}
\alias{mle1.GTDL}
\title{Maximum likelihood estimation}
\usage{
mle1.GTDL(start, t, method = "BFGS")
}
\arguments{
\item{start}{Initial values for the parameters to be optimized over.}

\item{t}{non-negative random variable representing the failure time and leave the snapshot failure rate, or danger.}

\item{method}{The method to be used.}
}
\value{
Returns a list of summary statistics
of the fitted GTDL distribution.
}
\description{
Estimate of the parameters.
}
\examples{

# times data (from Aarset, 1987))
data(artset1987)
mod <- mle1.GTDL(c(1,-0.05,-1),t = artset1987)

}
\references{
\itemize{
\item Aarset, M. V. (1987). How to Identify a Bathtub Hazard Rate. IEEE Transactions on Reliability, 36, 106–108.
\item Mackenzie, G. (1996) Regression Models for Survival Data: The Generalized Time-Dependent Logistic Family. Journal of the Royal Statistical Society. 
Series D (The Statistician). 45. 21-34.
}
}
\seealso{
\code{\link{optim}}
}
