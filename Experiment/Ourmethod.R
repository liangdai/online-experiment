library(cubature)

binomial_comp = function(p1,p2,n1,n2,N1=0,N2=0,R1=0,R2=0){
ans = pnorm(((R1+p1*n1)/(n1+N1)-(R2+p2*n2)/(n2+N2))/sqrt(n1/(n1+N1)^2*p1*(1-p1)+n2/(n2+N2)^2*p2*(1-p2)))
return(ans)
}

compute.ereward = function(R1,N1,R2,N2,F,p1,T){
myfun <- function(x){
	p = binomial_comp(x[1],x[2],floor(p1*T+0.5),T-floor(p1*T+0.5),N1,N2,R1,R2)
	ans = dbeta(x[1],R1,N1-R1)*dbeta(x[2],R2,N2-R2)*((p*x[1]+(1-p)*x[2])*F)
	return(ans)
	}
adaptIntegrate(myfun,rep(0,2), rep(1,2))$integral+R1/N1*p1*T + R2/N2*(1-p1)*T
}



prob4 = function(R1,N1,R2,N2,time,stage_2){
low = 0
high = 1
mid = 0.5
midleft = 0.25
midright = 0.75
rwd = rep(NA,5)
rwd[1] = compute.ereward(R1,N1,R2,N2,time,low,stage_2)
rwd[2] = compute.ereward(R1,N1,R2,N2,time,midleft,stage_2)
rwd[3] = compute.ereward(R1,N1,R2,N2,time,mid,stage_2)
rwd[4] = compute.ereward(R1,N1,R2,N2,time,midright,stage_2)
rwd[5] = compute.ereward(R1,N1,R2,N2,time,high,stage_2)

while((high-low)>0.02){
	max_idx = which.max(rwd)
	if(max_idx==1){
		high = midleft
		rwd[5] = rwd[2]
		mid = (low+high)/2
		midleft = (low+mid)/2
		midright = (mid+high)/2
		rwd[2] = compute.ereward(R1,N1,R2,N2,time,midleft,stage_2)
		rwd[3] = compute.ereward(R1,N1,R2,N2,time,mid,stage_2)
		rwd[4] = compute.ereward(R1,N1,R2,N2,time,midright,stage_2)
	}
	if(max_idx==2){
		high = mid
		mid = midleft
		midleft = (low+mid)/2
		midright = (mid+high)/2
		rwd[5] = rwd[3]
		rwd[3] = rwd[2]
		rwd[2] = compute.ereward(R1,N1,R2,N2,time,midleft,stage_2)
		rwd[4] = compute.ereward(R1,N1,R2,N2,time,midright,stage_2)
	}
	if(max_idx==3){
		low = midleft
		high = midright
		midleft = (low+mid)/2
		midright = (mid+high)/2
		rwd[1] = rwd[2]
		rwd[5] = rwd[4]
		rwd[2] = compute.ereward(R1,N1,R2,N2,time,midleft,stage_2)
		rwd[4] = compute.ereward(R1,N1,R2,N2,time,midright,stage_2)
	}
	if(max_idx==4){
		low = mid
		mid = midright
		midleft = (low+mid)/2
		midright = (mid+high)/2
		rwd[1] = rwd[3]
		rwd[3] = rwd[4]
		rwd[2] = compute.ereward(R1,N1,R2,N2,time,midleft,stage_2)
		rwd[4] = compute.ereward(R1,N1,R2,N2,time,midright,stage_2)
	}
	if(max_idx==5){
		low = midright
		rwd[1] = rwd[4]
		mid = (low+high)/2
		midleft = (low+mid)/2
		midright = (mid+high)/2
		rwd[2] = compute.ereward(R1,N1,R2,N2,time,midleft,stage_2)
		rwd[3] = compute.ereward(R1,N1,R2,N2,time,mid,stage_2)
		rwd[4] = compute.ereward(R1,N1,R2,N2,time,midright,stage_2)
	}
}

max_idx = which.max(rwd)
return(c(low,midleft,mid,midright,high)[max_idx])
}

correct_rpm = rep(0,num_iter)

for(iter in 1:num_iter){
print(iter)

#alpha1_store[iter,1] = control_size*p1[iter]
#beta1_store[iter,1]  = control_size - alpha1_store[iter,1]
alpha1_store[iter,1] = floor(control_size*p1[iter]+0.5)
beta1_store[iter,1]  = control_size-alpha1_store[iter,1]
if(alpha1_store[iter,1]==0){
	alpha1_store[iter,1] = 1
	beta1_store[iter,1]  = control_size-1
}

alpha1_store[iter,1] = 1
beta1_store[iter,1]  = 1

alpha2_store[iter,1] = 1
beta2_store[iter,1] = 1  

idx_1 = 0 
idx_2 = 0
stop = 0

for(i in 1:num_time){
	if(stop<1){
		prob_1 = prob4(alpha1_store[iter,i],alpha1_store[iter,i]+beta1_store[iter,i],alpha2_store[iter,i],alpha2_store[iter,i]+beta2_store[iter,i],(num_time-i+1)*(pos_time+num_time-i)*num_batch,(num_time-i+1)*num_batch)
		print(c(alpha1_store[iter,i],alpha1_store[iter,i]+beta1_store[iter,i],alpha2_store[iter,i],alpha2_store[iter,i]+beta2_store[iter,i],prob_1))
		if(((alpha1_store[iter,i]+beta1_store[iter,i])>=(alpha2_store[iter,i]+beta2_store[iter,i]))&&(prob_1==1))
			stop = 1
		if(((alpha1_store[iter,i]+beta1_store[iter,i])<=(alpha2_store[iter,i]+beta2_store[iter,i]))&&(prob_1==0))
			stop = 1
	}
	#stop = 0
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
	correct_rpm[iter] =  1
	post_loss[iter] = 0
}

cum_rew[iter] = sum(data1[iter,1:idx_1])+sum(data2[iter,1:idx_2])
if(((alpha1_store[iter,num_time+1]/beta1_store[iter,num_time+1])>=(alpha2_store[iter,num_time+1]/beta2_store[iter,num_time+1]))==(p1[iter]>=p2[iter]))
	post_cum_rew[iter] = rbinom(1,size=pos_time * num_batch, prob=p1[iter])
else
	post_cum_rew[iter] = rbinom(1,size=pos_time * num_batch, prob=p2[iter])
}

print(sum(correct_rpm))
#984
#
print(mean(cum_rew))
#95.397
#
print(mean(post_cum_rew))
print(mean(cum_rew+post_cum_rew))
print(mean(rowSums(loss_rpm)+post_loss))
print(mean(rowSums(loss_rpm)))
print(mean(post_loss))


