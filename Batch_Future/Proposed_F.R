inf = function(n,a,b){
	x=0:n
	#ans = sum(beta(a+x,b+n-x)*a(x,n,a,b)/(n+1)/beta(1+x,1+n-x)/beta(a,b))
	ans = sum(exp(lbeta(a+x,b+n-x)-lbeta(1+x,1+n-x)-lbeta(a,b)-log(n+1)))
	return(ans)
}
compute.prbopt3<-function(alpha1,beta1,alpha2,beta2){
		f <- function(x){
			r = x*pbeta(x,alpha2,beta2)-pbeta(x,alpha2+1,beta2)*alpha2/(beta2+alpha2)
			r = r*dbeta(x,alpha1,beta1)
			return (r)
			}
		ans= integrate(f,0,1)$value
	return(ans)
}

library(cubature)

binomial_comp = function(p1,p2,n1,n2,N1=0,N2=0,R1=0,R2=0){
i = 0:n1
ans = sum(dbinom(i,n1,p1)*pbinom(ceiling((i+R1)*(n2+N2+1)/(n1+N1+1)-1)-R2,n2,p2))
return(ans)
}

compute.ereward = function(R1,N1,R2,N2,F,p1,T){
myfun <- function(x){
	if(x[1]<x[2])
		ans = dbeta(x[1],R1,N1-R1)*dbeta(x[2],R2,N2-R2)*((1-binomial_comp(x[2],x[1],floor(p1*T),T-floor(p1*T),N1,N2,R1,R2))*(x[2]-x[1])*F+floor(p1*T)*(x[2]-x[1]))
	else
		ans = dbeta(x[1],R1,N1-R1)*dbeta(x[2],R2,N2-R2)*((1-binomial_comp(x[1],x[2],T-floor(p1*T),floor(p1*T),N2,N1,R2,R1))*(x[1]-x[2])*F+(T-floor(p1*T))*(x[1]-x[2]))
	return(ans)
	}
adaptIntegrate(myfun,rep(0,2), rep(1,2),tol =0.001)
}


#compute.ereward(12,beta1_store[iter,i],alpha2_store[iter,i],beta2_store[iter,i],pos_time*num_batch,probm,(num_time-i+1)*num_batch)


correct_rpm = 0
for(iter in 1:num_iter){
print(iter)

alpha1_store[iter,1] = 1
beta1_store[iter,1]  = 1

alpha2_store[iter,1] = 1
beta2_store[iter,1] = 1  

idx_1 = 0 
idx_2 = 0
stop = 0

for(i in 1:num_time){
	min_r = 999999
	prob_1 = 0
	for(probm in seq(0,1,by = 0.1)){
		exp_l =compute.ereward(alpha1_store[iter,i],alpha1_store[iter,i]+beta1_store[iter,i],alpha2_store[iter,i],alpha2_store[iter,i]+beta2_store[iter,i],(num_time-i)*num_batch,probm,num_batch)$integral
		if(exp_l<min_r){
			prob_1 = probm
			min_r = exp_l
		}
		
	}
	n_p1 = rbinom(1,size=num_actual_batch[i], prob=prob_1)
	n_p2 = num_actual_batch[i] - n_p1
	s = 0
	if(n_p1>0)
		s = sum(data1[iter,idx_1:(idx_1+n_p1-1)])
	alpha1_store[iter,i+1] = alpha1_store[iter,i] + s
	beta1_store[iter,i+1] = beta1_store[iter,i] + n_p1 - s
	idx_1 = idx_1 + n_p1 
	s = 0
	if(n_p2>0)
		s = sum(data2[iter,idx_2:(idx_2+n_p2-1)])
	alpha2_store[iter,i+1] = alpha2_store[iter,i] + s
	beta2_store[iter,i+1] = beta2_store[iter,i] + n_p2 - s
	idx_2 = idx_2 + n_p2 
	if(p1[iter]>=p2[iter])
		loss_rpm[iter,i] = n_p2*(p1[iter]-p2[iter])
	else
		loss_rpm[iter,i] = n_p1*(p2[iter]-p1[iter])
	
}
post_loss[iter] = pos_time * num_batch *abs(p1[iter]-p2[iter])
if(((alpha1_store[iter,num_time+1]/beta1_store[iter,num_time+1])>(alpha2_store[iter,num_time+1]/beta2_store[iter,num_time+1]))==(p1[iter]>=p2[iter])){
	correct_rpm = correct_rpm + 1
	post_loss[iter] = 0
	}
cum_rew[iter] = sum(data1[iter,1:idx_1])+sum(data2[iter,1:idx_2])
if(prob_1>=0.5)
	post_cum_rew[iter] = rbinom(1,size=pos_time * num_batch, prob=p1[iter])
else
	post_cum_rew[iter] = rbinom(1,size=pos_time * num_batch, prob=p2[iter])
}

print(correct_rpm)
print(mean(cum_rew))
print(mean(post_cum_rew))
print(mean(cum_rew+post_cum_rew))
print(mean(rowSums(loss_rpm)+post_loss))
print(mean(rowSums(loss_rpm)))
print(mean(post_loss))
ci = 0
for(i in 1:num_iter){
ci = ci + sqrt(alpha1_store[i,num_time+1]*beta1_store[i,num_time+1]/(alpha1_store[i,num_time+1]+beta1_store[i,num_time+1])^3+
	alpha2_store[i,num_time+1]*beta2_store[i,num_time+1]/(alpha2_store[i,num_time+1]+beta2_store[i,num_time+1])^3)
}
print(ci/num_iter)

pow = function(alpha1,beta1,alpha2,beta2,z){
	p1 = alpha1/(alpha1+beta1)
	p2 = alpha2/(alpha2+beta2)
	sd = sqrt(p1*(1-p1)/(alpha1+beta1)+p2*(1-p2)/(alpha2+beta2))
	p = (alpha1+alpha2)/(alpha1+beta1+alpha2+beta2)
	sd_m = sqrt(p*(1-p)/(alpha1+beta1)+p*(1-p)/(alpha2+beta2))
	q1 = (qnorm(1-z/2)*sd_m-(p1-p2))/sd
	q2 = (-qnorm(1-z/2)*sd_m-(p1-p2))/sd
	ans = 1-pnorm(q1)+pnorm(q2)
	return(ans)
}

ci = function(alpha1,beta1,alpha2,beta2){
	p1 = alpha1/(alpha1+beta1)
	p2 = alpha2/(alpha2+beta2)
	sd = sqrt(p1*(1-p1)/(alpha1+beta1)+p2*(1-p2)/(alpha2+beta2))
	return(sd)
} 

pro_exp_power = pow(alpha1_store[,],beta1_store[,],alpha2_store[,],beta2_store[,],0.05)
pro_exp_ci = ci(alpha1_store[,],beta1_store[,],alpha2_store[,],beta2_store[,])

pro_winprob = matrix(0,num_iter,num_time+1) 
for(i in 1:num_iter)
for(j in 1:(num_time+1)){
pro_winprob[i,j] = max(compute.win.prob(c(alpha1_store[i,j]-1,alpha2_store[i,j]-1),c(alpha1_store[i,j]+beta1_store[i,j]-1,alpha2_store[i,j]+beta2_store[i,j]-1),1000))
}

PRO_Power = colMeans(pro_exp_power)
PRO_CI = colMeans(pro_exp_ci)
PRO_Winprob = colMeans(pro_winprob)
PRO_Power_mean = mean(pro_exp_power[,(num_time+1)])
PRO_Power_sd = sd(pro_exp_power[,(num_time+1)])
PRO_CI_mean = mean(pro_exp_ci[,(num_time+1)])
PRO_CI_sd = sd(pro_exp_ci[,(num_time+1)])
PRO_Winprob_mean = mean(pro_winprob[,(num_time+1)])
PRO_Winprob_sd = sd(pro_winprob[,(num_time+1)])
PRO_SIZE1 = colMeans(alpha1_store+beta1_store)
PRO_SIZE2 = colMeans(alpha2_store+beta2_store)
