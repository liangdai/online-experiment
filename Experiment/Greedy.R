crtrate_Greedy = rep(NA,itertimes)
exp_reg_Greedy = rep(NA,itertimes)
post_reg_Greedy = rep(NA,itertimes)
sum_reg_Greedy = rep(NA,itertimes)

for(ldai in 1:itertimes){
correct_rpm = rep(0,num_iter)

for(iter in 1:num_iter){

alpha1_store[iter,1] = floor(control_size*p1[iter]+0.5)
beta1_store[iter,1]  = control_size-alpha1_store[iter,1]

#alpha1_store[iter,1] = 0
#beta1_store[iter,1]  = 0

alpha2_store[iter,1] = 0
beta2_store[iter,1] = 0  

idx_1 = 0 
idx_2 = 0
stop = 0

for(i in 1:num_time){
	if((alpha1_store[iter,i]==0)||(alpha2_store[iter,i]==0))
		prob_1=0.5
	else
		prob_1 = (alpha1_store[iter,i]/beta1_store[iter,i])>=(alpha2_store[iter,i]/beta2_store[iter,i])
	#if (stop==0)
		#prob_1 = (prob_1-0.5)*(1-(num_time-i)/num_time*0.5)+0.5
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

if(((alpha1_store[iter,num_time+1]/beta1_store[iter,num_time+1])>=(alpha2_store[iter,num_time+1]/beta2_store[iter,num_time+1]))==(p1[iter]>=p2[iter])){
	correct_rpm[iter] = 1
	post_loss[iter] = 0
}

cum_rew[iter] = sum(data1[iter,1:idx_1])+sum(data2[iter,1:idx_2])
if(((alpha1_store[iter,num_time+1]/beta1_store[iter,num_time+1])>=(alpha2_store[iter,num_time+1]/beta2_store[iter,num_time+1]))==(p1[iter]>=p2[iter]))
	post_cum_rew[iter] = rbinom(1,size=pos_time * num_batch, prob=p1[iter])
else
	post_cum_rew[iter] = rbinom(1,size=pos_time * num_batch, prob=p2[iter])
}

crtrate_Greedy[ldai]=(sum(correct_rpm))
sum_reg_Greedy[ldai]=(mean(rowSums(loss_rpm)+post_loss))
exp_reg_Greedy[ldai]=(mean(rowSums(loss_rpm)))
post_reg_Greedy[ldai]=(mean(post_loss))
}

print(mean(crtrate_Greedy))
print(mean(sum_reg_Greedy))


