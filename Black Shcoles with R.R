###  Find the Option Greeks, B-S model prices, and replicationg portfolios
### For Puts, set isPut=1

BScall <- function(t=0,T,S,K,r,q=0,sigma,isPut=0) {
 # t and T are measured in years; all parameters are annualized # q is the continuous dividend yield
	d1 <- (log(S/K)+(r-q+sigma^2/2)*(T-t))/(sigma*sqrt(T-t))
 d2 <- d1-sigma*sqrt(T-t)
 binary <- pnorm(-d2)*exp(-r*T)
 
 # Call Greeks at t
 Delta <- exp(-q*(T-t))*pnorm(d1)
 Gamma <- exp(-q*(T-t))*exp(-d1^2/2)/sqrt(2*pi)/S/sigma/sqrt(T-t)
 Vega <- S*exp(-q*(T-t))/sqrt(2*pi)*exp(-d1^2/2)*sqrt(T-t)
Theta <- -S*exp(-q*(T-t))*sigma/sqrt(T-t)/2*dnorm(d1) - r*K*exp(-r*(T-t))*pnorm(d2) +
    q*S*exp(-q*(T-t))*pnorm(d1)
 Rho <- (T-t)*K*exp(-r*(T-t))*pnorm(d2)
 
 # Black-Scholes formula for Calls
 BSprice <- -K*exp(-r*(T-t))*pnorm(d2)+S*Delta
 if (isPut==1) {
 Delta <- -exp(-q*(T-t))*pnorm(-d1)
 BSprice <- S*Delta+K*exp(-r*(T-t))*pnorm(-d2)
 Theta <- -S*exp(-q*(T-t))*sigma/sqrt(T-t)/2*dnorm(d1) + r*K*exp(-r*(T-t))*pnorm(-d2)-q*S*exp(-q*(T-t))*pnorm(-d1)
 Rho<- - (T-t)*K*exp(-r*(T-t))*pnorm(-d2)
 }
 Bank<-BSprice-Delta*S
	return(list(Delta=Delta,Gamma=Gamma,Theta=Thera,Vega=Vega,Rho=Rho,Price=BSprice,d1=d1,d2=d2,B=Bank))
	}