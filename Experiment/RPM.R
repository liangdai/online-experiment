crtrate_RPM = rep(NA,itertimes)
exp_reg_RPM = rep(NA,itertimes)
post_reg_RPM = rep(NA,itertimes)
sum_reg_RPM = rep(NA,itertimes)

for(ldai in 1:itertimes){
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

sim.post<-function(y,n,ndraws){
	k<-length(y)
	ans = matrix(NA,ndraws,2)
	no = n - y
	for(i in 1:k)
		ans[,i] = rbeta(ndraws,y[i]+1,no[i] + 1)
	return(ans)
}

prob.winner<-function(post){
	k<-ncol(post)
	w<-table(factor(max.col(post),levels = 1:k))
	return(w/sum(w))
}
compute.win.prob<-function(y,n,ndraws){
	return(prob.winner(sim.post(y,n,ndraws)))
}

correct_rpm = rep(0,num_iter)

for(iter in 1:num_iter){


#alpha1_store[iter,1] = control_size*p1[iter]
#beta1_store[iter,1]  = control_size - alpha1_store[iter,1]
alpha1_store[iter,1] = floor(control_size*p1[iter]+0.5)
beta1_store[iter,1]  = control_size-alpha1_store[iter,1]
if(alpha1_store[iter,1]==0){
	alpha1_store[iter,1] = 1
	beta1_store[iter,1]  = control_size-1
}

#alpha1_store[iter,1] = 1
#beta1_store[iter,1] = 1  

alpha2_store[iter,1] = 1
beta2_store[iter,1] = 1  

idx_1 = 0 
idx_2 = 0
stop = 0

for(i in 1:num_time){
	if(stop<1){
		prob_1 = compute.win.prob(c(alpha1_store[iter,i]-1,alpha2_store[iter,i]-1),c(alpha1_store[iter,i]+beta1_store[iter,i]-1,alpha2_store[iter,i]+beta2_store[iter,i]-1),1000)[1]
		#print(c(alpha1_store[iter,i],alpha1_store[iter,i]+beta1_store[iter,i],alpha2_store[iter,i],alpha2_store[iter,i]+beta2_store[iter,i],prob_1))
		if ((prob_1 >=0.95)&&(i>1)){
			stop = 1
			prob_1 = 1
		}
		else if (prob_1<0.05){
			stop = 1
			prob_1 = 0
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

prob_1 = compute.win.prob(c(alpha1_store[iter,(num_time+1)]-1,alpha2_store[iter,(num_time+1)]-1),c(alpha1_store[iter,(num_time+1)]+beta1_store[iter,(num_time+1)]-1,alpha2_store[iter,(num_time+1)]+beta2_store[iter,(num_time+1)]-1),1000)[1]
if((prob_1>=0.5)==(p1[iter]>=p2[iter])){
	correct_rpm[iter] = 1
	post_loss[iter] = 0
}

cum_rew[iter] = sum(data1[iter,1:idx_1])+sum(data2[iter,1:idx_2])
if(prob_1>=0.5)
	post_cum_rew[iter] = rbinom(1,size=pos_time * num_batch, prob=p1[iter])
else
	post_cum_rew[iter] = rbinom(1,size=pos_time * num_batch, prob=p2[iter])
}

crtrate_RPM[ldai]=(sum(correct_rpm))
sum_reg_RPM[ldai]=(mean(rowSums(loss_rpm)+post_loss))
exp_reg_RPM[ldai]=(mean(rowSums(loss_rpm)))
post_reg_RPM[ldai]=(mean(post_loss))
print(ldai)
}

print(mean(crtrate_RPM))
print(mean(sum_reg_RPM))

