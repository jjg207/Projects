### This script can generate the tree for European style (set isAm =0) and American style (set isAm =1). 

### Binomial tree recursively for a Put payoff.

### For Call, modify the payoff function.

binTreeAm<-function(T=20/52,S=49, K=50,N=10,r=0.05,delta=0,sigma=0.2,u=NULL,d=NULL,isAm=1) {
	dt <- T/N
	
	if (is.null(u)){
		u<-exp((r-delta)*dt + sigma*sqrt(dt))
		d <- exp( (r-delta)*dt - sigma*sqrt(dt))
		}
		
	disc <- exp(-r*dt)
	q <- (exp((r-delta)*dt)-d)/(u-d)

	V <- array(0,dim=c(N+1,N+1)) # matrix for storing all the option values
	Del <- array(0,dim=c(N+1,N+1))
	
	for (i in 0:(N)){ # terminal payoff
		curS<- S*(u)^i*(d)^(N-i)
		V[N+1,i+1]<-max(K-curS,0)
	}
	
	for (j in (N):1){
		for (i in 0:(j-1)){
			V[j,i+1]<-disc*(q*V[j+1,i+2]+(1-q)*V[j+1,i+1])
			if (isAm == 1) #if yes, then the option is American
			V[j,i+1]<-max(K-S*(u)^(i)*(d)^(j-i-1),V[j,i+1]) # compare the intrinsic value with the expected value
			Del[j,i+1]=exp(-delta*dt)*(V[j+1,i+2]-V[j+1,i+1])/(S*(u)^(i+1)*(d)^(j-1-i)-S*(u)^(i)*(d)^(j-1))
		}
	}
	
	return(list(premium=V[1,1],allDelta=Del,allV=V))
}
