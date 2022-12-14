% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit2.R
\name{mle2.GTDL}
\alias{mle2.GTDL}
\title{Maximum likelihood estimates of the GTDL model}
\usage{
mle2.GTDL(t, start, formula, censur, method = "BFGS")
}
\arguments{
\item{t}{non-negative random variable representing the failure time and leave the snapshot failure rate, or danger.}

\item{start}{Initial values for the parameters to be optimized over.}

\item{formula}{The structure matrix of covariates of dimension n x p.}

\item{censur}{censoring status 0=censored, a=fail.}

\item{method}{The method to be used.}
}
\value{
Returns a list of summary statistics
of the fitted GTDL model.
}
\description{
Maximum likelihood estimates of the GTDL model
}
\examples{

### Example 1

require(survival)
data(lung)

lung <- lung[-14,]
lung$ph.ecog[lung$ph.ecog==3]<-2
t1 <- lung$time
start1 <- c(0.03,0.05,-1,0.7,2,-0.1)
formula1 <- ~lung$sex+factor(lung$ph.ecog)+lung$age
censur1 <- ifelse(lung$status==1,0,1)
fit.model1 <- mle2.GTDL(t = t1,start = start1,
                     formula = formula1,
                     censur = censur1)
fit.model1

### Example 2

data(tumor)
t2 <- tumor$time
start2 <- c(1,-0.05,1.7)
formula2 <- ~tumor$group
censur2 <- tumor$censured
fit.model2 <- mle2.GTDL(t = t2,start = start2,
                       formula = formula2,
                       censur = censur2)
fit.model2

### Example 3

data(SP_covid)
attach(SP_covid)
t3 <- time_symptoms
start3 <- c(1,1,-1,-1,1,1,-1,1)
censur3 <- censure
formula3 <- ~sex + icu + obesity + diabetes + cardiopathy + asthma
fit.model3 <- mle2.GTDL(t = t3, start = start3, formula = formula3,
censur = censur3)


fit.model3

}
\references{
\itemize{
\item Mackenzie, G. (1996) Regression Models for Survival Data: The Generalized Time-Dependent Logistic Family. Journal of the Royal Statistical Society. 
Series D (The Statistician). (45). 21-34.
}
}
\seealso{
\code{\link{optim}}
}
