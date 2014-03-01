binomial_comp = function(p1,p2,T,delta){
i = 0:(T/2)
ans = sum(dbinom(i,T/2,p1)*pbinom(i-delta,T/2,p2))
return(ans)
}

compute.ereward = function(R1,N1,R2,N2,F,T,P){
	
myfun <- function(x){	
	p = binomial_comp(x[1],x[2],T,R2-R1,P)
	ans = dbeta(x[1],R1,N1-R1)*dbeta(x[2],R2,N2-R2)*((p*x[1]+(1-p)*x[2])*F + mean(x)*T)
	return(ans)
	}
return(max(R1/N1,R2/N2)*(F+T)-adaptIntegrate(myfun,rep(0,2), rep(1,2),tol =0.001)$integral)
}

compute.prbopt<-function(y,n){
	k = length(y)
	ans = numeric(k)
	for(i in 1:k){
		idx = (1:k)[-i]
		f <- function(x){
			r = dbeta(x,y[i]+1,n[i]-y[i]+1)
			for(j in idx)
				r = r*pbeta(x,y[j]+1,n[j]-y[j]+1)
			return (r)
			}
		ans[i] = integrate(f,0,1)$value
	}
	return(ans)
}

binomial_comp = function(p1,p2,n1,n2,N1=0,N2=0,R1=0,R2=0){
i = 0:n1
#R.NEW = ceiling(R)
ans = sum(dbinom(i,n1,p1)*pbinom(ceiling((i+R1)*(n2+N2)/(n1+N1))-R2,n2,p2))
return(ans)
}

binomial_comp = function(p1,p2,n1,n2,N1=0,N2=0,R1=0,R2=0){
ans = pnorm(((R1+p1*n1)/(n1+N1)-(R2+p2*n2)/(n2+N2))/sqrt(n1/(n1+N1)^2*p1*(1-p1)+n2/(n2+N2)^2*p2*(1-p2)))
return(ans)
}

binomial_comp(0.1,0.12,100,100)
binomial_comp2(0.1,0.12,100,100)

library(cubature)
compute.ereward = function(R1,N1,R2,N2,F,p1,T){
myfun <- function(x){
	p = binomial_comp(x[1],x[2],floor(p1*T+0.5),T-floor(p1*T+0.5),N1,N2,R1,R2)
	ans = dbeta(x[1],R1,N1-R1)*dbeta(x[2],R2,N2-R2)*((p*x[1]+(1-p)*x[2])*F + R1/N1*p1*T + R2/N2*(1-p1)*T)
	return(ans)
	}
adaptIntegrate(myfun,rep(0,2), rep(1,2),tol =0.001)$integral
}


prob2 = function(R1,N1,R2,N2,time,stage_2){
max_rwd = -1 
max_p1 = 0
for( p in seq(0,1,by =0.01)){
	rwd = compute.ereward(R1,N1,R2,N2,time-stage_2,p,stage_2)
	if(rwd>max_rwd){
		max_rwd = rwd
		max_p1 = p
	}		
}
return(max_p1)
}
n = 100
Winp.Matrix = array(dim=c(n,n,n,n))
for(i in 2:n)
	for(j in 1:(i-1))
		for(k in 2:n){
			print(i)
			Winp.Matrix[i,k,j,(floor(j*k/i)):1] = 0
			for(l in floor(j*k/i):1){
				tmp = prob2(l,k,j,i,1000,100)
				Winp.Matrix[i,k,j,l] = tmp
				if(tmp==0)
					break
			}
			Winp.Matrix[i,k,j,(floor(j*k/i)):n] = 1
			for(l in floor(j*k/i):(k-1)){
				tmp = prob2(l,k,j,i,1000,100)
				Winp.Matrix[i,k,j,l] = tmp
				if(tmp==0)
					break
			}
	}

prob2(2,10,30,100,10000,1000)

num_prob = 100
time = 2000
stage_1 = 300
stage_2 = 300*2
p = runif(num_prob, 0.15, 0.20)
q = runif(num_prob, 0.15, 0.20)
data1 = matrix(NA, num_prob,stage_1+stage_2)
data2 = matrix(NA, num_prob,stage_1+stage_2)

for(i in 1:num_prob){
data1[i,] = rbinom(stage_1+stage_2,size=1,prob=p[i])
data2[i,] = rbinom(stage_1+stage_2,size=1,prob=q[i])
}

sum = 0
for(i in 1:num_prob){
	#print (i)
	R1 = 1 + sum(data1[i,1:stage_1])
	N1 = 2 + stage_1
	
	R2 = 1 + sum(data2[i,1:stage_1])
	N2 = 2 + stage_1
	max_p1=compute.prbopt(c(R1-1,R2-1),c(N1-2,N2-2))[1]
	
	#max_p1 = prob2(R1,N1,R2,N2,time-stage_1,stage_2)
	#print(max_p1-compute.prbopt(c(R1-1,R2-1),c(N1-2,N2-2))[1])
	#max_p1 =0.5
	#print(max_p1)
	num1 = max_p1*stage_2
	num2 = (1-max_p1)*stage_2
	dec = 0
	if(mean(data1[i,1:(stage_1+num1)])>=mean(data2[i,1:(stage_1+num2)])){
		f_sum = (time-stage_1-stage_2)*p[i]
		dec = 1 
	}
	else{
		f_sum = (time-stage_1-stage_2)*q[i]
		dec = 2
	}
	sum = sum + sum(data1[i,1:(stage_1+num1)])+sum(data2[i,1:(stage_1+num2)])+f_sum
}

sum