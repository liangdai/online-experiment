inf = function(n,a,b){
	x=0:n
	#ans = sum(beta(a+x,b+n-x)*a(x,n,a,b)/(n+1)/beta(1+x,1+n-x)/beta(a,b))
	ans = (exp(lbeta(a+x,b+n-x)-lbeta(1+x,1+n-x)-lbeta(a,b)-log(n+1)))
	return(ans)
}
inf(3,4,6)%*%t(inf(4,4,6))


compute.prbopt3<-function(alpha1,beta1,alpha2,beta2){
		f <- function(x){
			r = x*pbeta(x,alpha2,beta2)-pbeta(x,alpha2+1,beta2)*alpha2/(beta2+alpha2)
			r = r*dbeta(x,alpha1,beta1)
			return (r)
			}
		ans= integrate(f,0,1)$value
	return(ans)
}




correct_rpm = 0
for(iter in 1:num_iter){
print(iter)

alpha1_store[iter,1] = control_size*p1[iter]
beta1_store[iter,1]  = control_size - alpha1_store[iter,1]

alpha2_store[iter,1] = 1
beta2_store[iter,1] = 1  

idx_1 = 0 
idx_2 = 0
stop = 0

for(i in 1:num_time){
	min_r = 999999
	for(s_ctl in 1:num_batch){
		inf(s_ctl,alpha1_store[iter,i],beta1_store[iter,i])%*%inf(num_batch-s_ctl,alpha2_store[iter,i],beta2_store[iter,i])
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
ci = 0
for(i in 1:num_iter){
ci = ci + sqrt(alpha1_store[i,num_time+1]*beta1_store[i,num_time+1]/(alpha1_store[i,num_time+1]+beta1_store[i,num_time+1])^3+
	alpha2_store[i,num_time+1]*beta2_store[i,num_time+1]/(alpha2_store[i,num_time+1]+beta2_store[i,num_time+1])^3)
}
print(ci/num_iter)

