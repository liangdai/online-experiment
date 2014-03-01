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

binomial_comp = function(p1,p2,n1,n2,N1=0,N2=0,R1=0,R2=0){
ans = pnorm(((R1+p1*n1)/(n1+N1)-(R2+p2*n2)/(n2+N2))/sqrt(n1/(n1+N1)^2*p1*(1-p1)+n2/(n2+N2)^2*p2*(1-p2)))
return(ans)
}

library(cubature)
compute.ereward = function(R1,N1,R2,N2,F,p1,T){
myfun <- function(x){
	p = binomial_comp(x[1],x[2],floor(p1*T+0.5),T-floor(p1*T+0.5),N1,N2,R1,R2)
	ans = dbeta(x[1],R1,N1-R1)*dbeta(x[2],R2,N2-R2)*((p*x[1]+(1-p)*x[2])*F)
	return(ans)
	}
adaptIntegrate(myfun,rep(0,2), rep(1,2))$integral+R1/N1*p1*T + R2/N2*(1-p1)*T
}

prob2 = function(R1,N1,R2,N2,time,stage_2){
max_rwd = -1 
max_p1 = 0
for( p in seq(0,1,by =0.01)){
	rwd = compute.ereward(R1,N1,R2,N2,time,p,stage_2)
	if(rwd>=(max_rwd-0.0000001)){
		max_rwd = rwd
		max_p1 = p
	}		
}
return(max_p1)
}

num_prob = 10000
time = seq(0,40000,by=1000)
stage_1_1 = 250
stage_1_2 = 250 
stage_2 = 500

p = runif(num_prob, 0.18, 0.18)
q = runif(num_prob, 0.06, 0.06)

data1 = matrix(NA, num_prob,stage_1_1+stage_2)
data2 = matrix(NA, num_prob,stage_1_2+stage_2)



for(i in 1:num_prob){
data1[i,] = rbinom(stage_1_1+stage_2,size=1,prob=p[i])
data2[i,] = rbinom(stage_1_2+stage_2,size=1,prob=q[i])
}

max_p1c=c()
reward= rep(NA,length(time))
correct = rep(0,length(time))
for(tt in 1:length(time)){
print(tt)
sum = 0
for(i in 1:num_prob){
	#print (i)
	R1 =  sum(data1[i,1:stage_1_1])+1
	N1 =  stage_1_1+2
	
	R2 =  sum(data2[i,1:stage_1_2])+1
	N2 = stage_1_2+2
	#max_p1=compute.prbopt(c(R1-1,R2-1),c(N1-2,N2-2))[1]
	
	max_p1 = prob4(R1,N1,R2,N2,time[tt],stage_2)
	#print(max_p1)
	#print(max_p1-compute.prbopt(c(R1-1,R2-1),c(N1-2,N2-2))[1])
	#max_p1 =0.5
	#print(max_p1)
	#max_p1 = (((R1-1)/(N1-2))>=((R2-1)/(N2-2)))
	num1 = max_p1*stage_2
	num2 = (1-max_p1)*stage_2
	dec = 0
	if(mean(data1[i,1:(stage_1_1+num1)])>=mean(data2[i,1:(stage_1_2+num2)])){
		f_sum = time[tt]*p[i]
		dec = 1 
		if(p[i]>=q[i]){
			correct[tt] = correct[tt] + 1
			#cc[i]=1
		}
		
	}
	else{
		f_sum = time[tt]*q[i]
		dec = 2
		if(p[i]<q[i]){
			correct[tt] = correct[tt] + 1
			#cc[i]=1
		}
	}
	#f_sum = time[tt]*p[i]
	
	sum = sum + sum(data1[i,1:(stage_1_1+num1)])+sum(data2[i,1:(stage_1_2+num2)])+f_sum
}
reward[tt] = (sum/num_prob)
print(correct[tt])
print(reward[tt])
}


reward_new = reward
#104.1200 180.2900 259.8935 418.1700 495.7600 575.5200 655.4000 734.1300
correct_new = correct
# 86 88 91 94 94 94 95 95

reward_50 = reward
 [1]   70.19  148.79  227.39  305.99  384.59  463.19  541.79  620.39  698.99
[10]  777.59  856.19  934.79 1013.39 1091.99 1170.59 1249.19 1327.79 1406.39
[19] 1484.99 1563.59 1642.19 1720.79 1799.39 1877.99 1956.59 2035.19 2113.79
[28] 2192.39 2270.99 2349.59 2428.19 2506.79 2585.39 2663.99 2742.59 2821.19
[37] 2899.79 2978.39 3056.99 3135.59 3214.19
correct_50 = correct
# 93

reward_100 = reward
[1]   73.79  150.79  227.79  304.79  381.79  458.79  535.79  612.79  689.79
[10]  766.79  843.79  920.79  997.79 1074.79 1151.79 1228.79 1305.79 1382.79
[19] 1459.79 1536.79 1613.79 1690.79 1767.79 1844.79 1921.79 1998.79 2075.79
[28] 2152.79 2229.79 2306.79 2383.79 2460.79 2537.79 2614.79 2691.79 2768.79
[37] 2845.79 2922.79 2999.79 3076.79 3153.79
correct_100 = correct
#85

max_reward = reward
# 76.45 116.45 156.45 196.45 236.45 276.45 316.45 356.45 396.45


#100-0       52.24000  58.79790  65.35581 85.02952  117.81905 183.43088 708.03047 1363.82095 3331.19236 6610.14473 13168.04945 32841.76363 65631.28726
#50-50       45.06900  51.66509  58.26118 78.04944  111.02989 177.02375 704.67785 1364.28671 3343.11327 6641.15754 13237.24609 33025.51172 66005.95443
#RPM         80.85700  87.33373  93.81045 113.24063 145.62426 210.42390 728.52958 1376.20216 3319.21989 6557.58278 13034.30855 32464.48588 64848.11476
#NEW METHOD  81.18900  87.67412  94.11219 113.56732 145.91809 210.86442 728.68816 1377.96501 3323.28556 6564.68335 13042.39819 32484.22869 64860.23207

50-50
 [1]    73.3100   150.9100   228.5488   849.3100  1625.3100  3953.3100
 [7]  7833.3100 15593.3100 38873.3100 77673.3100
 [1] 88 88 88 88 88 88 88 88 88 88

100-0
 [1]    76.270   150.270   224.307   816.270  1556.270  3776.270  7476.270
 [8] 14876.270 37076.270 74076.270
[1] 70 70 70 70 70 70 70 70 70 70

RPM 
[1]    75.540   151.540   227.578   835.540  1595.540  3875.540  7675.540
 [8] 15275.540 38075.540 76075.540
[1] 80 80 80 80 80 80 80 80 80 80


new
[1]    76.270   152.270   228.908   847.480  1617.600  3937.760  7777.610
 [8] 15517.570 38777.480 77477.390
[1] 70 79 80 85 85 86 85 86 87 87

arrays=c()
for(i in 1:1000)
arrays = c(arrays,((sum(rbinom(500,size=1,prob=0.08))+1)/502)>=((sum(rbinom(50,size=1,prob=0.06))+1)/52))

mean(arrays)

for(i in c(500,1000,1500,2000,2500,3000,3500,4000))
	print(prob2(18,252,16,252,i,500))
	
prob2(R1,N1,R2,N2,10000,stage_2)
prob2(R1,N1,R2,N2,20000,stage_2)
prob2(R1,N1,R2,N2,30000,stage_2)
prob2(R1,N1,R2,N2,40000,stage_2)




binomial_comp2 = function(p1,p2,n,p,N1=0,N2=0,R1=0,R2=0){
ab1 = (R1+p1*n*p)/(n*p+N1)
ab2 = (R2+p2*n*(1-p))/(n*(1-p)+N2)
bl = n*p/(n*p+N1)^2*p1*(1-p1)+n*(1-p)/(n*(1-p)+N2)^2*p2*(1-p2)

tmp11 = p1*n/(n*p+N1)-(R1+p1*n*p)*n/(n*p+N1)^2
tmp12 = -p2*n/(n*(1-p)+N2)+(R2+p2*n*(1-p))*n/(n*(1-p)+N2)^2
t1 = n/(n*p+N1)^2-2*n^2*p/(n*p+N1)^3
t2 = -n/(n*(1-p)+N2)^2+2*n^2*(1-p)/(n*(1-p)+N2)^3
tmp2 = -0.5*bl^(-1.5)*(p1*(1-p1)*t1+p2*(1-p2)*t2)
tmp = ab1*tmp2-ab2*tmp2+(tmp11-tmp12)/sqrt(bl)
ans = dnorm((ab1-ab2)/sqrt(bl))*tmp
return(ans)
}


compute.ereward2 = function(R1,N1,R2,N2,F,p1,T){
myfun <- function(x){
	p = binomial_comp2(x[1],x[2],T,p1,N1,N2,R1,R2)
	ans = dbeta(x[1],R1,N1-R1)*dbeta(x[2],R2,N2-R2)*(p*F*(x[1]-x[2]) )
	return(ans)
}
adaptIntegrate(myfun,rep(0,2), rep(1,2))$integral
#adaptIntegrate(myfun,rep(0,2), rep(1,2))$integral - (R1/N1-R2/N2)*T
}

prob3 = function(R1,N1,R2,N2,time,stage_2){
low=0
high =1
mid = 0.5
targt = (R1/N1-R2/N2)*stage_2
rwd_low = compute.ereward2(R1,N1,R2,N2,time,low,stage_2)
rwd_high = compute.ereward2(R1,N1,R2,N2,time,high,stage_2)
low_flag = (rwd_low+targt)<0

while((high-low)>0.02){
	print(c(low,mid, high))
	rwd_mid = compute.ereward2(R1,N1,R2,N2,time,mid,stage_2)
	if(xor((low_flag),(rwd_mid+targt)<0)){
		high = mid
		mid = (low+high)/2
	}
	else{
		low = mid
		mid = (low+high)/2
	}
}
return(mid)
}
ptm <- proc.time()
prob2(18,252,16,252,40000,500)
print(proc.time() - ptm)
ptm <- proc.time()
prob3(18,252,16,252,40000,500)
print(proc.time() - ptm)
ptm <- proc.time()
prob4(18,252,16,252,40000,500)
print(proc.time() - ptm)


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

while((high-low)>0.01){
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


for(i in 1:num_prob){
	#print (i)
	R1 =  sum(data1[i,1:stage_1_1])+1
	N1 =  stage_1_1+2
	
	R2 =  sum(data2[i,1:stage_1_2])+1
	N2 = stage_1_2+2
	OUTPUT = prob4(R1,N1,R2,N2,400000,500)
	print(c(R1,N1,R2,N2,OUTPUT))
}

prob4(23,252,12,252,4000000,500)