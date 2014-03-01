num_batch = 100
num_time = 20
pos_time = 800
num_iter = 1000
control_size = 1000
p1 = rep(0,num_iter)
p2 = rep(0,num_iter)
data1 = matrix(NA,num_iter,num_time*num_batch*1.5)
data2 = matrix(NA,num_iter,num_time*num_batch*1.5)
num_actual_batch = rpois(num_time,num_batch)
for(i in 1:num_iter){
	p = runif(2, 0, 0.1)
	p1[i] = p[1]
	p2[i] = p[2]
	data1[i,] = rbinom(num_time*num_batch*1.5,size=1, prob=p1[i])
	data2[i,] = rbinom(num_time*num_batch*1.5,size=1, prob=p2[i])
}

	