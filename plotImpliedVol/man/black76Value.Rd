\name{black76Value}
\alias{black76Value}

\title{
Black76Value computation
}
\description{
Black76Value computation
}
\usage{
black76Value(insOption, sigma)
}

\arguments{
  \item{insOption}{df rows}
  \item{sigma}{ volatility}
}
\details{
Black76Value computation
}
\value{
  \item{Fx }{target function}
  \item{comp2 }{Description of 'comp2'}
}

\author{Charles Cheng}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (insOption, sigma) 
{
    X = as.numeric(insOption["strike"])
    S = as.numeric(insOption["futurePrice"])
    TTM = as.numeric(insOption["time_to_expiry"])
    type = insOption["type"]
    optVal = as.numeric(insOption["optionPrice"])
    if (is.na(insOption["rfRate"])) {
        rfRate = 0
    }
    else {
        rfRate = as.numeric(insOption["rfRate"])
    }
    if (type == "C") {
        indicator = 1
    }
    else {
        indicator = -1
    }
    sigmaT = sigma * sqrt(TTM)
    d1 = (log(S/X) + (rfRate + sigma^2/2) * TTM)/sigmaT
    d2 = d1 - sigmaT
    Strikerf = X * exp(-rfRate * TTM)
    B76Val = (pnorm(d1 * indicator) * S - pnorm(d2 * indicator) * 
        Strikerf) * indicator
    d2prime = d1/sigma
    d1prime = sqrt(TTM) + d2prime
    B76ValPrime = d1prime * dnorm(d1) * S - d2prime * dnorm(d2) * 
        Strikerf
    return(list(Fx = (B76Val - optVal), DFx = B76ValPrime))
  }
}


\keyword{ black76 }

