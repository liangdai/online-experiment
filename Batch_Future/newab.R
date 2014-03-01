a_mean = 0.045
b_mean = 0.041
n = 10000000
a_num = 5000000
b_num = n - a_num
start_num = 200

a = rbinom(n=a_num,size=1, prob=a_mean) 
b = rbinom(n=b_num,size=1, prob=b_mean)
a[1]=b[1]=1

N=90000


a_idx_method1 = start_num
b_idx_method1 = start_num
num_method1 = start_num*2
while (num_method1<N){
	num_method1 = num_method1 + 1
	theta_a_method1 = mean(a[1:a_idx_method1])
	theta_b_method1 = mean(b[1:b_idx_method1])
	mle_method1 = theta_a_method1 - theta_b_method1	
	if((theta_a_method1+sqrt(2*log(num_method1)/a_idx_method1))>(theta_b_method1+sqrt(2*log(num_method1)/b_idx_method1)))
		a_idx_method1 = a_idx_method1 + 1
	else
		b_idx_method1 = b_idx_method1 + 1
}
revenue1 = sum(a[1:a_idx_method1])+sum(b[1:b_idx_method1])
delq_method1 = sqrt(theta_a_method1*(1-theta_a_method1)/a_idx_method1 + theta_b_method1*(1-theta_b_method1)/b_idx_method1)


a_idx_method2 = start_num
b_idx_method2 = start_num
num_method2 = start_num*2
while (num_method2<N){
	num_method2 = num_method2 + 1
	theta_a_method2 = mean(a[1:a_idx_method2])
	theta_b_method2 = mean(b[1:b_idx_method2])
	mle_method2 = theta_a_method2 - theta_b_method2	
	if(rbinom(1,size=1, prob=0.5))
		a_idx_method2 = a_idx_method2 + 1
	else
		b_idx_method2 = b_idx_method2 + 1
}
revenue2 = sum(a[1:a_idx_method2])+sum(b[1:b_idx_method2])
delq_method2 = sqrt(theta_a_method2*(1-theta_a_method2)/a_idx_method2 + theta_b_method2*(1-theta_b_method2)/b_idx_method2)

a_idx_method3 = start_num
b_idx_method3 = start_num
num_method3 = start_num*2
while (num_method3<N){
	num_method3 = num_method3 + 1
	theta_a_method3 = mean(a[1:a_idx_method3])
	theta_b_method3 = mean(b[1:b_idx_method3])
	mle_method3 = theta_a_method3 - theta_b_method3	
	if((a_idx_method3/b_idx_method3)<sqrt(theta_a_method3*(1-theta_a_method3)/(theta_b_method3*(1-theta_b_method3))))
		a_idx_method3 = a_idx_method3 + 1
	else
		b_idx_method3 = b_idx_method3 + 1
}
revenue3 = sum(a[1:a_idx_method3])+sum(b[1:b_idx_method3])
delq_method3 = sqrt(theta_a_method3*(1-theta_a_method3)/a_idx_method3 + theta_b_method3*(1-theta_b_method3)/b_idx_method3)

a_idx_method4 = start_num
b_idx_method4 = start_num
num_method4 = start_num*2
T1 = 0.002
while (num_method4<N){
	num_method4 = num_method4 + 1
	theta_a_method4 = mean(a[1:a_idx_method4])
	theta_b_method4 = mean(b[1:b_idx_method4])
	mle_method4 = theta_a_method4 - theta_b_method4	
	if(theta_a_method4>theta_b_method4){
		if(sqrt(theta_a_method4*(1-theta_a_method4)/(N-b_idx_method4) + theta_b_method4*(1-theta_b_method4)/b_idx_method4)<T1)
			a_idx_method4 = a_idx_method4 + 1
		else{
			if(rbinom(1,size=1, prob=0.5))
				a_idx_method4 = a_idx_method4 + 1
			else
				b_idx_method4 = b_idx_method4 + 1
		}
	}
	else{
		if(sqrt(theta_a_method4*(1-theta_a_method4)/a_idx_method4 + theta_b_method4*(1-theta_b_method4)/(N-a_idx_method4))<T1)
			b_idx_method4 = b_idx_method4 + 1
		else{
			if(rbinom(1,size=1, prob=0.5))
				a_idx_method4 = a_idx_method4 + 1
			else
				b_idx_method4 = b_idx_method4 + 1
		}
	}
}
revenue4 = sum(a[1:a_idx_method4])+sum(b[1:b_idx_method4])
delq_method4 = sqrt(theta_a_method4*(1-theta_a_method4)/a_idx_method4 + theta_b_method4*(1-theta_b_method4)/b_idx_method4)

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

a_idx_method5 = start_num
b_idx_method5 = start_num
num_method5 = start_num*2
while (num_method5<N){
	num_method5 = num_method5 + 1
	theta_a_method5 = sum(a[1:a_idx_method5])
	theta_b_method5 = sum(b[1:b_idx_method5])
	if(rbinom(1,size=1, prob=compute.prbopt(c(theta_a_method5,theta_b_method5),c(a_idx_method5,b_idx_method5))[1]))
		a_idx_method5 = a_idx_method5 + 1
	else
		b_idx_method5 = b_idx_method5 + 1
}
revenue5 = sum(a[1:a_idx_method5])+sum(b[1:b_idx_method5])
delq_method5 = sqrt(theta_a_method5*(a_idx_method5-theta_a_method5)/(a_idx_method5^3) + theta_b_method5*(b_idx_method5-theta_b_method5)/(b_idx_method5^3))
sum(compute.prbopt(c(theta_a_method5,theta_b_method5),c(a_idx_method5,b_idx_method5)))

print(c(revenue1,revenue2,revenue3,revenue4,revenue5))
print(c(delq_method1,delq_method2,delq_method3,delq_method4,delq_method5))




f = function(a,b,r) 1/(1-r)*a/(a+b)
m = matrix(0,201,201)
for(i in 1:200){
	m[i,201-i] = f(i,201-i,0.9)
}

m=matrix(0,7,7)
m[1,] =c(0.7029,0.800,0.8452,0.8723,0.8905,0.9039,0.9141)
m[2,] = c( 0.5001,0.6346, 0.7072, 0.7539, 0.7869, 0.8115, 0.8307)
m[3,]=c( 0.3796, 0.5163, 0.6010, 0.6579, 0.6996, 0.7318, 0.7573)
m[4,]=c( 0.3021, 0.4342, 0.5184, 0.5809, 0.6276, 0.6642, 0.6940)
m[5,]=c( 0.2488, 0.3720, 0.4561, 0.5179, 0.5676, 0.6071, 0.6395)
m[6,]=c( 0.2103, 0.3245, 0.4058, 0.4677, 0.5168, 0.5581, 0.5923)
m[7,] = c(0.1815, 0.2871, 0.3647, 0.4257, 0.4748, 0.5156, 0.5510)
m = m*10
n = matrix(0,7,7)
for(i in 1:6)
	for(j in 1:6){
	#n[i,j] = max(i/(i+j)/0.1, i/(i+j)+ 0.9*i/(i+j)*m[i+1,j]+0.9*j/(i+j)*m[i,j+1])
	n[i,j] = i/(i+j)+ 1*(i/(i+j)*m[i+1,j]+j/(i+j)*m[i,j+1])
}

numc = function(p1,p2,beta){
	p = (p1+p2)/2
	delta1 = sqrt(2*p*(1-p))
	delta2 = sqrt(p1*(1-p1)+p2*(1-p2))
	n = ((delta2*qnorm(1-beta)+delta1*qnorm(1-0.05/2))/abs(p1-p2))^2
	return(n)
}
numc(0.05,0.04,0.05)

compute.prbopt2<-function(alpha,beta,N){
	ans = numeric(N+1)
	for(x in 0:N){
		f <- function(theta){
			r = dbeta(theta,alpha,beta)*dbinom(x,N,theta)
			return (r)
			}
		ans[x+1] = integrate(f,0,1)$value
	}
	return(ans)
}

compute.prbopt3<-function(alpha1,beta1,alpha2,beta2){
		f <- function(x){
			r = x*pbeta(x,alpha2,beta2)-pbeta(x,alpha2+1,beta2)*alpha2/(beta2+alpha2)
			r = r*dbeta(x,alpha1,beta1)
			return (r)
			}
		ans= integrate(f,0,1)$value
	return(ans)
}

a = compute.prbopt2(100,100,5)
b = compute.prbopt2(100,100,4)
c = compute.prbopt2(4,6,1)
sum(a)

compute.prbopt3(500,500,100,100)
compute.prbopt3(4,6,100,100)
compute.prbopt3(4,6,10,10)

compute.prbopt3(100,100,500,500)

compute.prbopt3(4,6,100,100)
compute.prbopt3(8,12,100,100)
compute.prbopt3(4,6,105,105)

compute.prbopt3(100,100,4,6)

compute.prbopt3(1000,1000,4,6) + compute.prbopt3(4,7,1000,1000)*0.6 + compute.prbopt3(5,6,1000,1000)*0.4

compute.prbopt3(4,6,1000,1000) + compute.prbopt3(4,6,1001,1000)* 0.5 + compute.prbopt3(4,6,1000,1001)*0.5


compute.prbopt3(4,6,6,4) 

compute.prbopt3(1,1,1,1)

compute.prbopt3(2,3,6,7)

a= 0.4*abs(compute.prbopt3(3,3,6,7)-compute.prbopt3(2,3,6,7))+0.6*abs(compute.prbopt3(2,4,6,7)- compute.prbopt3(2,3,6,7))

b = 6/13*abs(compute.prbopt3(2,3,7,7)-compute.prbopt3(2,3,6,7))+7/13*abs(compute.prbopt3(2,4,6,8)- compute.prbopt3(2,3,6,7))
a-b
0.4-6/13

aproxim = function(s){
	if (s<= 0.2)
		r = sqrt(s/2)
	else if (s<=1)
		r = 0.49 - 0.11*s^(-0.5)
	else if (s<=5)
		r = 0.63 - 0.26*s^(-0.5)
	else if (s<=15)
		r = 0.77 - 0.58*s^(-0.5)
	else
		r = sqrt(2*log(s)-log(log(s))-log(16*pi))
	return(r)
}
ab = function(a,b,r){
	rest = a/(a+b) + sqrt(a*b/(a+b)^2/(a+b+1))*aproxim(-1/log(r)/(a+b+1))
	return(rest)
}
ab(100,0,0.99)

aproxim(-1/log(0.9)/2)*sqrt(1/8)

ab(4,6,0.9)
ab(500,500,0.9)

while(1){
p = runif(2, 0, 0.1)
p1 = p[1]
p2 = p[2]

N = 10000
theta = 0.99

alpha1 = 1000* p1
beta1 = 1000 - alpha1

alpha1 = 1
beta1 = 1
alpha2 = 1
beta2 = 1 
idx_1 = 0 
idx_2 = 0
r = rep(NA,N)

for(i in 1: N){
	if(ab(alpha1,beta1,theta)>=ab(alpha2,beta2,theta)){
		idx_1 = idx_1 + 1
		r[i] = rbinom(n=1,size=1, prob=p1)
		alpha1 = alpha1 + r[i]
		beta1 = beta1 + 1 - r[i]
	}
	else{
		idx_2 = idx_2 + 1
		r[i] = rbinom(n=1,size=1, prob=p2)
		alpha2 = alpha2 + r[i]
		beta2 = beta2 + 1 - r[i]
	}
}
if((ab(alpha1,beta1,theta)>=ab(alpha2,beta2,theta))==(p1>=p2))
	correct_num1= correct_num1 + 1
if(p1>=p2){
	loss1 = idx_2*(p1-p2)
	}
if(p1<p2){
	loss1= idx_1*(p2-p1)
	}
if(loss1>100)
	break;
}