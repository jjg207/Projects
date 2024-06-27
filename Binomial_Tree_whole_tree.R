binTree<-function(S=100, K=110, N=2, r=0.05, delta=0.03, u=1.1,d=0.8,h=0.5){
	discount_factor<-exp(-r*h)
	q<-(exp((r-delta)*h-d)/(u-d))	#risk neutral probability
	V<-array(0,dim=c(N+1,N+1))	#matrix for storing all option values at each node 
	#each row is one period, the printed matrices represent the lying down tree.
	
	
	Delta<-B<-array(0,dim=c(N+1,N+1))	#matrix for storing replicating portfolio at each node 
	for (i in 0:(N)){ #terminal payoff
		finalS<-S*(u)^i*(d)^(N-i)  #i-th value of ST
		V[N+1,i+1]<-max(finalS-K,0)	#associated option value
		}
	V_rnp=V #results from risk neutral
	
	for (j in (N):1){# column in the tree
		for (i in 0:(j-1)){ #i number of up-moves in the jth column
			currentS=S*u^(i)*d^(j-1-i)
			B[j,i+1]=discount_factor*(u*V[j+1,i+1] - d*V[j+1,i+2])/(u-d)
			Delta[j,i+1] = exp(-delta*h)*(V[j+1,i+2]-V[j+1,i+1])/(currentS*u-currentS*d)
			V[j,i+1]=Delta[j,i+1]*currentS+B[j,i+1]
			# or if you prefer the risk-neural method when (Delta,B)
			V_rnp[j,i+1] <-discount_factor*(q*V_rnp[j+1,i+2] + (1-q)*V_rnp[j+1,i+1])
			}
		}
		return(list(premia=V,Delta=Delta,Bank=B))
}