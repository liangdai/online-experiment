correct_rpm = 0

for(iter in 1:num_iter){


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
	pp1 = alpha1_store[iter,i]/(alpha1_store[iter,i]+beta1_store[iter,i])
	pp2 = alpha2_store[iter,i]/(alpha2_store[iter,i]+beta2_store[iter,i])
	pp_sum = (alpha1_store[iter,i]+beta1_store[iter,i])+(alpha2_store[iter,i]+beta2_store[iter,i])
	if((pp1+sqrt(2*log(pp_sum)/(alpha1_store[iter,i]+beta1_store[iter,i])))>=(pp2+sqrt(2*log(pp_sum)/(alpha2_store[iter,i]+beta2_store[iter,i]))))
		prob_1 = 1
	else
		prob_1 = 0
	
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

ucb1_exp_power = pow(alpha1_store[,],beta1_store[,],alpha2_store[,],beta2_store[,],0.05)
ucb1_exp_ci = ci(alpha1_store[,],beta1_store[,],alpha2_store[,],beta2_store[,])

ucb1_winprob = matrix(0,num_iter,num_time+1) 
for(i in 1:num_iter)
for(j in 1:(num_time+1)){
ucb1_winprob[i,j] = max(compute.win.prob(c(alpha1_store[i,j]-1,alpha2_store[i,j]-1),c(alpha1_store[i,j]+beta1_store[i,j]-1,alpha2_store[i,j]+beta2_store[i,j]-1),1000))
}

UCB1_Power = colMeans(ucb1_exp_power)
UCB1_CI = colMeans(ucb1_exp_ci)
UCB1_Winprob = colMeans(ucb1_winprob)
UCB1_Power_mean = mean(ucb1_exp_power[,(num_time+1)])
UCB1_Power_sd = sd(ucb1_exp_power[,(num_time+1)])
UCB1_CI_mean = mean(ucb1_exp_ci[,(num_time+1)])
UCB1_CI_sd = sd(ucb1_exp_ci[,(num_time+1)])
UCB1_Winprob_mean = mean(ucb1_winprob[,(num_time+1)])
UCB1_Winprob_sd = sd(ucb1_winprob[,(num_time+1)])
