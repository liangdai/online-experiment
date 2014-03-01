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
	prob_1 = 0.52
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

LIN_exp_power = pow(alpha1_store[,],beta1_store[,],alpha2_store[,],beta2_store[,],0.05)
LIN_exp_ci = ci(alpha1_store[,],beta1_store[,],alpha2_store[,],beta2_store[,])

LIN_winprob = matrix(0,num_iter,num_time+1) 
for(i in 1:num_iter)
for(j in 1:(num_time+1)){
LIN_winprob[i,j] = max(compute.win.prob(c(alpha1_store[i,j]-1,alpha2_store[i,j]-1),c(alpha1_store[i,j]+beta1_store[i,j]-1,alpha2_store[i,j]+beta2_store[i,j]-1),1000))
}

LIN_Power = colMeans(LIN_exp_power)
LIN_CI = colMeans(LIN_exp_ci)
LIN_Winprob = colMeans(LIN_winprob)
LIN_Power_mean = mean(LIN_exp_power[,(num_time+1)])
LIN_Power_sd = sd(LIN_exp_power[,(num_time+1)])
LIN_CI_mean = mean(LIN_exp_ci[,(num_time+1)])
LIN_CI_sd = sd(LIN_exp_ci[,(num_time+1)])
LIN_Winprob_mean = mean(LIN_winprob[,(num_time+1)])
LIN_Winprob_sd = sd(LIN_winprob[,(num_time+1)])

mean((alpha1_store[,num_time+1]+beta1_store[,num_time+1]-2)/(alpha2_store[,num_time+1]+beta2_store[,num_time+1]-2))


a = c()
start = 50
avg = 100

for(i in seq(start,avg, b=10))	{
	tmp1 = rep(i, i/10)
	tmp2 = rep(2*avg-i, i/10)
	a = c(a,tmp1,tmp2)
}


b = c()
start = 50
avg = 70
for(i in seq(start,avg, b=10))	{
	tmp1 = rep(i, i/10)
	tmp2 = rep(2*avg-i, i/10)
	b = c(b,tmp1,tmp2)
}

c = c()
start = 110
avg = 130
for(i in seq(start,avg, b=10))	{
	tmp1 = rep(i, (i-60)/10)
	tmp2 = rep(2*avg-i, (i-60)/10)
	c = c(c,tmp1,tmp2)
}

d = seq(50,100, b=10)
e = seq(100,150, b=10)
f = c(b,c,d,e,70,70,100,100,130,130)

a=rnorm(90,)
pain = c(a,f)
drug = c(rep("A",90), rep("B",90))
migraine = data.frame(pain,drug)
results = aov(pain ~ drug, data=migraine)
summary(results)

tempforshen = rep(0,10000)
for(tttt in 1:10000)
tempforshen[tttt] = mean(rbeta(100,1,1))

plot(density(tempforshen))
