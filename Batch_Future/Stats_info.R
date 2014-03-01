a=function(x,n,a,b){
	ans = lbeta(a,b)-lbeta(a+x,b+n-x)+x*(digamma(a+x)-digamma(a+b+n))+(n-x)*(digamma(b+n-x)-digamma(a+b+n))
	return(ans)
}

inf = function(n,a,b){
	x=0:n
	#ans = sum(beta(a+x,b+n-x)*a(x,n,a,b)/(n+1)/beta(1+x,1+n-x)/beta(a,b))
	ans = sum(exp(lbeta(a+x,b+n-x)-lbeta(1+x,1+n-x)-lbeta(a,b)-log(n+1))*a(x,n,a,b))
	return(ans)
}

correct_rpm = 0

for(iter in 1:num_iter){

print(iter)
#alpha1_store[iter,1] = control_size*p1[iter]
#beta1_store[iter,1]  = control_size - alpha1_store[iter,1]
alpha1_store[iter,1] = 1
beta1_store[iter,1]  = 1

alpha2_store[iter,1] = 1
beta2_store[iter,1] = 1  

idx_1 = 0 
idx_2 = 0
stop = 0

for(i in 1:num_time){
	
	max_v = 0
	max_id =-1
	nn = (num_time-i+1)*num_batch
	#for(id in 1:nn){
	#	l1 = inf(id,alpha1_store[iter,i],beta1_store[iter,i])
	#	l2 = inf(nn-id,alpha2_store[iter,i],beta2_store[iter,i])
	#	if((l1+l2)>max_v){
	#		max_v = l1+l2
	#		max_id =id
	#	}
		
	#}	
	#prob_1 = max_id/nn
	#prob_1 = min(1,max(0,prob_1))
	prob_1 =0.52
	
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
if((prob_1>=0.5)==(p1[iter]>=p2[iter])){
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

info_exp_power = pow(alpha1_store[,num_time+1],beta1_store[,num_time+1],alpha2_store[,num_time+1],beta2_store[,num_time+1],0.05)
info_exp_ci = ci(alpha1_store[,num_time+1],beta1_store[,num_time+1],alpha2_store[,num_time+1],beta2_store[,num_time+1])



info_winprob = rep(0,num_iter) 
for(idx in 1:num_iter){
info_winprob[idx] = max(compute.win.prob(c(alpha1_store[idx,num_time+1]-1,alpha2_store[idx,num_time+1]-1),c(alpha1_store[idx,num_time+1]+beta1_store[idx,num_time+1]-1,alpha2_store[idx,num_time+1]+beta2_store[idx,num_time+1]-1),1000))
}
mean(info_exp_power)
#0.684283
mean(info_exp_ci)
#0.009556742
print(mean(info_winprob))
#0.94519

