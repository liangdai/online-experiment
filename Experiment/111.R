num_time = 5
pos_time = 500
control_size = 250
num_batch = 100
num_iter = 100
itertimes =100
p1 = rep(0,num_iter)
p2 = rep(0,num_iter)
data1 = matrix(NA,num_iter,num_time*num_batch*1.5)
data2 = matrix(NA,num_iter,num_time*num_batch*1.5)


set.seed(10000)
num_actual_batch = rpois(num_time,num_batch)
for(i in 1:num_iter){
	p1[i] = runif(1, 0, 0.1)
	p2[i] = runif(1, 0, 0.1)
	data1[i,] = rbinom(num_time*num_batch*1.5,size=1, prob=p1[i])
	data2[i,] = rbinom(num_time*num_batch*1.5,size=1, prob=p2[i])
}

loss_rpm = matrix(NA,num_iter,num_time)
alpha1_store = matrix(NA,num_iter,num_time+1)
beta1_store = matrix(NA,num_iter,num_time+1)
alpha2_store = matrix(NA,num_iter,num_time+1)
beta2_store = matrix(NA,num_iter,num_time+1)
cum_rew = rep(NA,num_iter)
post_cum_rew = rep(NA,num_iter)
post_loss = rep(NA,num_iter)


