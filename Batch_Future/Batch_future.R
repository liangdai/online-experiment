a = rep(0,11)
for(i in 0:10)
a[i+1] = pbinom(i,10,0.4)-pbinom(i-1,10,0.4)

binomial_comp = function(p1,p2,n1,n2,N1=0,N2=0,R1=0,R2=0){
i = 0:n1
ans = sum(dbinom(i,n1,p1)*pbinom(ceiling((i+R1)*(n2+N2+1)/(n1+N1+1)-1)-R2,n2,p2))
return(ans)
}

binomial_comp(0.6,0.4,3500,1900)
binomial_comp(0.14,0.12,19,35)

a = rep(0,19) 
for(i in 1:19){
	a[i] = binomial_comp(0.16,0.14,i,20-i,40,40,2,2)
}

binomial_comp(0.16,0.14,10000,10000)
binomial_comp(0.16,0.14,20000,1)

compute.prbopt2<-function(alpha,beta,N){
	ans = numeric(N+1)
	for(x in 0:N){
		f <- function(theta){
			r = dbeta(theta,alpha,beta)*dbinom(x,N,theta)
			return (r)
			}
		ans[x+1] = integrate(f,0,1)$value
	}
	return(ans)
}

compute.ereward = function(R1,R2,N1,N2,F,p1,T){
myfun <- function(x,y){
	if(x<y)
		ans = dbeta(x,R1,N1-R1)*dbeta(y,R2,N2-R2)*(binomial_comp(x,y,floor(p1*T),T-floor(p1*T),N1,N2,R1,R2)*(y-x)*F+floor(p1*T)*(y-x))
	else
		ans = dbeta(x,R1,N1-R1)*dbeta(y,R2,N2-R2)*(binomial_comp(y,x,T-floor(p1*T),floor(p1*T),N2,N1,R2,R1)*(x-y)*F+floor(p1*T)*(x-y))
	return(ans)
	}
llim <- 0
ulim <- 1
integrate(function(y) { 
   sapply(y, function(y) {
     integrate(function(x) myfun(x,y), llim, ulim)$value
   })
 }, llim, ulim)$value
}

compute.ereward(30,50,120,170,1000,0.01,500)


compute.ereward = function(R1,R2,N1,N2,F,p1,T){
myfun <- function(x){
	if(x[1]<x[2])
		ans = dbeta(x[1],R1,N1-R1)*dbeta(x[2],R2,N2-R2)*(binomial_comp(x[1],x[2],floor(p1*T),T-floor(p1*T),N1,N2,R1,R2)*(x[2]-x[1])*F+floor(p1*T)*(x[2]-x[1]))
	else
		ans = dbeta(x[1],R1,N1-R1)*dbeta(x[2],R2,N2-R2)*(binomial_comp(x[2],x[1],T-floor(p1*T),floor(p1*T),N2,N1,R2,R1)*(x[1]-x[2])*F+floor(p1*T)*(x[1]-x[2]))
	return(ans)
	}
adaptIntegrate(myfun,rep(0,2), rep(1,2))
}

compute.ereward(3,50,12,170,1000,0.05,500)


compute.ereward = function(R1,R2,N1,N2,F,p1,T){
myfun <- function(x){
	ans = dbeta(x[1],R1,N1-R1)*dbeta(x[2],R2,N2-R2)
	return(ans)
	}
adaptIntegrate(myfun,rep(0,2), rep(1,2),tol =0.001)
}
compute.ereward(300,5000,2000,10000,100000,0.05,50000)

compute.ereward(11,988,1,2,80000,0.05,1000)

compute.ereward = function(R1,R2,N1,N2,F,p1,T){
myfun <- function(x){
	if(x[1]<x[2])
		ans = dbeta(x[1],R1,N1-R1)*dbeta(x[2],R2,N2-R2)*(binomial_comp(x[1],x[2],floor(p1*T),T-floor(p1*T),N1,N2,R1,R2)*(x[2]-x[1])*F+floor(p1*T)*(x[2]-x[1]))
	else
		ans = dbeta(x[1],R1,N1-R1)*dbeta(x[2],R2,N2-R2)*(binomial_comp(x[2],x[1],T-floor(p1*T),floor(p1*T),N2,N1,R2,R1)*(x[1]-x[2])*F+floor(p1*T)*(x[1]-x[2]))
	return(ans)
	}
adaptIntegrate(myfun,rep(0,2), rep(1,2),tol =0.001)
}


compute.ereward = function(R1,N1,R2,N2,F,p1,T){
myfun <- function(x){
	if(x[1]<x[2])
		ans = dbeta(x[1],R1,N1-R1)*dbeta(x[2],R2,N2-R2)*(binomial_comp(x[1],x[2],floor(p1*T),T-floor(p1*T),N1,N2,R1,R2)*(x[2]-x[1])*F+floor(p1*T)*(x[2]-x[1]))
	else
		ans = dbeta(x[1],R1,N1-R1)*dbeta(x[2],R2,N2-R2)*(binomial_comp(x[2],x[1],T-floor(p1*T),floor(p1*T),N2,N1,R2,R1)*(x[1]-x[2])*F+(T-floor(p1*T))*(x[1]-x[2]))
	return(ans)
	}
adaptIntegrate(myfun,rep(0,2), rep(1,2),tol =0.001)
}
compute.ereward(99,1000,48,1200,80000,0,8800)$integral