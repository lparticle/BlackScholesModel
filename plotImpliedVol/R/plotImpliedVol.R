black76Value = function(insOption, sigma){
	X=as.numeric(insOption["strike"])
	S=as.numeric(insOption["futurePrice"])
	TTM=as.numeric(insOption["time_to_expiry"])
	type=insOption["type"]
	optVal = as.numeric(insOption["optionPrice"])

	##if df does not contain rfRate, then risk-free-rate default to zero.
	if(is.na(insOption["rfRate"])){
		rfRate=0
	}else{
		rfRate = as.numeric(insOption["rfRate"]) 
	}
	
	#using indicator to integrate both Call & Put options in the same form
	if(type == "C"){
		indicator = 1
	} else {
		indicator = -1
	}

    	sigmaT  = sigma*sqrt(TTM)
    	d1      = (log(S/X) + (rfRate + sigma^2/2)*TTM)/sigmaT
    	d2      = d1 - sigmaT

    	Strikerf= X*exp(-rfRate*TTM)

    	B76Val = (pnorm(d1*indicator)*S - pnorm(d2*indicator)*Strikerf)*indicator

    	d2prime = d1/sigma
    	d1prime = sqrt(TTM) + d2prime
    	B76ValPrime = d1prime*dnorm(d1)*S - d2prime*dnorm(d2)*Strikerf
    	
	return(list(Fx =(B76Val-optVal), DFx=B76ValPrime))
  }


nrSolve = function(insOption,initVol=1, tol=1.e-7, maxInterations=100){
	##Newton-Raphson method for finding the root of an equation is well documented. 
	##insOption, single row/option of df
	##initVol, initial volatility, default is 1 	

	count = 0
	delta = 0
	vol = initVol

	#cat("Running Newton-Raphson method\n Iteration  vol            Fx         DFx        delta\n")
 
  	while(count<maxInterations){
    		## evaluate target function and its derivative
		black76 = black76Value(insOption, vol)	
		
		if(is.nan(black76$Fx)){
			##no convergence
			black76$conv = -1
			return(vol)
		}

    		if( abs(black76$Fx)<tol ){
      		## got a root optimal solution within maximum iterations
      		black76$conv = count
      		return(vol)
    		}
		#cat(sprintf("%3d   % .3e   % .1e   % .1e   % .1e\n", count, vol, black76$Fx, black76$DFx, delta))

		count = count+1		
		delta = black76$Fx / black76$DFx
		
		## update current guess for root
    		vol   = vol - delta
  	}

  	## no convergence
  	black76$conv = -1
  	return(vol)
}


plotImpliedVol = function(df) { 
   	##df is data frame type. columns are as below:
	##strike = c(50, 20), the option strike - in $ 
	##type = c("C", "P"), either "c" for  call option or "p" for a put option 
	##optionPrice = c(1.62,0.01), the option price - in $ 
	##futurePrice = c(48.03, 48.03),the price of the underlying future - in $ 
	##time_to_expiry = c(0.1423, 0.1423),the option time to expiry - in year 
	##rfRate, if df does not contains such columns, then risk-free-rate default to zero.

	#par(xpd=FALSE)
	impliedVol=apply(df, 1, nrSolve) #initVol,tol,maxSteps
	
	##print volatility results
	#cat(sprintf("Implied Volatilities of European Options %d is: %.2f%%\n",as.numeric(rownames(df)),impliedVol*100))
	
	##plot implied volatility vs strike price.
	plot(df$strike,impliedVol*100, col=ifelse(df$type=="C","red","blue"), 
		xlab="Strike $", ylab="Implied Volatility %", main="Black76 Implied Volatility",
		pch=2)
	legend("topright",pch=c(2,2), col=c("red", "blue"), c("Call Option", "Put Option"))
	grid(NULL,NULL,col = "lightgray")

	return(impliedVol) ## return impliedVol
}