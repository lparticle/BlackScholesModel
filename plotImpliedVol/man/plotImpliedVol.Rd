\name{plotImpliedVol}
\alias{plotImpliedVol}
\title{Black76 Implied Volatility}

\description{
compute & plot Black76 implied volatility for European options
}

\usage{
plotImpliedVol(df)
}

\arguments{
  \item{df}{data.frame format with 5 or 6 columns: 
	1.df$strike: the option strike -in $;  
	2.df$type: option type: "C" for call and "P" for put;  
	3.df$optionPrice: the option price -in $; 
	4.df$futurePrice: the price of the underlying future -in $;  
  	5.df$time_to_expiry: the option time to expiry -in year;  
  	6.df$rfRate the risk-free-rate, optional input.
	}
}

\value{return the Black76 implied volatilities}

\author{Charles Cheng}

\examples{
df=data.frame(strike=c(50,20), type=c("C","P"), optionPrice=c(1.62,0.01), futurePrice=c(48.03,48.03), time_to_expiry=c(0.1423,0.1423))
plotImpliedVol(df)

df=data.frame(strike=c(50,20), type=c("C","P"), optionPrice=c(1.62,0.01), futurePrice=c(48.03,48.03), time_to_expiry=c(0.1423,0.1423), rfRate=c(0.01,0.01))
plotImpliedVol(df)
}

\keyword{ Black76 }
\keyword{ European Option }