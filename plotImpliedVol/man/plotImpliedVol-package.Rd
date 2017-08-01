\name{plotImpliedVol-package}
\alias{plotImpliedVol-package}
\docType{package}
\title{
\packageTitle{plotImpliedVol}
}
\description{
\packageDescription{plotImpliedVol}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{plotImpliedVol}
\packageIndices{plotImpliedVol}
compute & plot Black76 implied volatility
}
\author{
\packageAuthor{plotImpliedVol}

Maintainer: \packageMaintainer{plotImpliedVol}
}

\keyword{ package }

\examples{
df=data.frame(strike=c(50,20), type=c("C","P"), optionPrice=c(1.62,0.01), futurePrice=c(48.03,48.03), time_to_expiry=c(0.1423,0.1423))
plotImpliedVol(df)

df=data.frame(strike=c(50,20), type=c("C","P"), optionPrice=c(1.62,0.01), futurePrice=c(48.03,48.03), time_to_expiry=c(0.1423,0.1423), rfRate=c(0.01,0.01))
plotImpliedVol(df)
}
