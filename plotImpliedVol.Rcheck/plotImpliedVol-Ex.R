pkgname <- "plotImpliedVol"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('plotImpliedVol')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("black76Value")
### * black76Value

flush(stderr()); flush(stdout())

### Name: black76Value
### Title: Black76Value computation
### Aliases: black76Value
### Keywords: black76

### ** Examples

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



cleanEx()
nameEx("nrSolve")
### * nrSolve

flush(stderr()); flush(stdout())

### Name: nrSolve
### Title: Newton solution
### Aliases: nrSolve
### Keywords: Newton-Raphson

### ** Examples

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



cleanEx()
nameEx("plotImpliedVol-package")
### * plotImpliedVol-package

flush(stderr()); flush(stdout())

### Name: plotImpliedVol-package
### Title: Black76 Implied Volatility
### Aliases: plotImpliedVol-package
### Keywords: package

### ** Examples

df=data.frame(strike=c(50,20), type=c("C","P"), optionPrice=c(1.62,0.01), futurePrice=c(48.03,48.03), time_to_expiry=c(0.1423,0.1423))
plotImpliedVol(df)

df=data.frame(strike=c(50,20), type=c("C","P"), optionPrice=c(1.62,0.01), futurePrice=c(48.03,48.03), time_to_expiry=c(0.1423,0.1423), rfRate=c(0.01,0.01))
plotImpliedVol(df)



cleanEx()
nameEx("plotImpliedVol")
### * plotImpliedVol

flush(stderr()); flush(stdout())

### Name: plotImpliedVol
### Title: Black76 Implied Volatility
### Aliases: plotImpliedVol
### Keywords: Black76 European Option

### ** Examples

df=data.frame(strike=c(50,20), type=c("C","P"), optionPrice=c(1.62,0.01), futurePrice=c(48.03,48.03), time_to_expiry=c(0.1423,0.1423))
plotImpliedVol(df)

df=data.frame(strike=c(50,20), type=c("C","P"), optionPrice=c(1.62,0.01), futurePrice=c(48.03,48.03), time_to_expiry=c(0.1423,0.1423), rfRate=c(0.01,0.01))
plotImpliedVol(df)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
