binomCall<- function(K,n=4,T=1,S0=100,r=0.04,u=1.2,d=0.9) {
  h <- T/n
  q <- (exp(r*h)-d)/(u-d)
  Sbin <- S0*d^n*(u/d)^(0:n) 	#(n+1) values for ST
  qbin <- dbinom(0:n,n,q)		#corresponding probabilities for each value of ST, which is binomial distribution
  Payoff <- pmax(Sbin-K,0)	#use payoff function find the Call value for the nodes in the last column
  avePayoff <- sum(qbin*Payoff)		# use RNP find premium
  return(exp(-r*T)*avePayoff) }