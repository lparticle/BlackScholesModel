\name{nrSolve}
\alias{nrSolve}

\title{
Newton solution
}
\description{
Newton solution
}
\usage{
nrSolve(insOption, initVol = 1, tol = 1e-07, maxInterations = 100)
}


\arguments{
  \item{insOption}{df row}
  \item{initVol}{initial volatility}
  \item{tol}{tolerance}
  \item{maxInterations}{maximum iteration}
}
\details{
Newton-Raphson method
}
\value{
volatility
}

\author{
Charles Cheng
}


\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (insOption, initVol = 1, tol = 1e-07, maxInterations = 100) 
{
    count = 0
    delta = 0
    vol = initVol
    while (count < maxInterations) {
        black76 = black76Value(insOption, vol)
        if (is.nan(black76$Fx)) {
            black76$conv = -1
            return(vol)
        }
        if (abs(black76$Fx) < tol) {
            black76$conv = count
            return(vol)
        }
        count = count + 1
        delta = black76$Fx/black76$DFx
        vol = vol - delta
    }
    black76$conv = -1
    return(vol)
  }
}

\keyword{ Newton-Raphson }

