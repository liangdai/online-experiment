binomial_comp = function(p1,p2,n1,n2,N1=0,N2=0,R1=0,R2=0){
i = 0:n1
ans = rowSums(sapply(i,dbinom,size=n1,prob=p1)*sapply(ceiling((i+R1)*(n2+N2+1)/(n1+N1+1)-1)-R2,pbinom,size=n2,prob=p2))
return(ans)
}
compute.ereward.sampling = function(R1,N1,R2,N2,F,p1,T,ndraws){
lambda1 = rbeta(ndraws,R1,N1-R1)
lambda2 = rbeta(ndraws,R2,N2-R2)
max.exp.rewd = apply(rbind(lambda1,lambda2),2,max)*(F+T)
p1.win = binomial_comp(lambda1,lambda2,floor(p1*T),T-floor(p1*T),N1,N2,R1,R2)
p2.win = 1 - p1.win
cur.exp.rewd = floor(p1*T)*lambda1 + (T-floor(p1*T))*lambda2 + p1.win*lambda1*F + p2.win*lambda2*F
return(mean(max.exp.rewd - cur.exp.rewd))
}
compute.ereward.sampling(80,8000,91,9000,1000,0.9,500,100000)


binomial_comp(c(0.3,0.5),c(0.4,0.7),200,400)
binomial_comp(c(0.3,0.3),c(0.4,0.4),200,400)

####################################################################################
binomial_comp = function(p1,p2,n1,n2,N1=0,N2=0,R1=0,R2=0){
i = 0:n1
ans = sum(dbinom(i,n1,p1)*pbinom(ceiling((i+R1)*(n2+N2+1)/(n1+N1+1)-1)-R2,n2,p2))
return(ans)
}

compute.ereward = function(R1,N1,R2,N2,F,p1,T){
myfun <- function(x){
max.exp.rewd = max(x)*(F+T)
p1.win = binomial_comp(x[1],x[2],floor(p1*T),T-floor(p1*T),N1,N2,R1,R2)
p2.win = 1 - p1.win
cur.exp.rewd = floor(p1*T)*x[1] + (T-floor(p1*T))*x[2] + p1.win*x[1]*F + p2.win*x[2]*F
return((max.exp.rewd -cur.exp.rewd )*dbeta(x[1],R1,N1-R1)*dbeta(x[2],R2,N2-R2))
}
adaptIntegrate(myfun,rep(0,2), rep(1,2),tol=0.001)
}
compute.ereward(80,8000,90,8900,1000,0.9,500)

####################################################################################
binomial_comp = function(p1,p2,n1,n2,N1=0,N2=0,R1=0,R2=0){
i = 0:n1
ans = rowSums(sapply(i,dbinom,size=n1,prob=p1)*sapply(ceiling((i+R1)*(n2+N2+1)/(n1+N1+1)-1)-R2,pbinom,size=n2,prob=p2))
return(ans)
}
compute.ereward2 = function(R1,N1,R2,N2,F,p1,T){
myfun <- function(x,y){
max.exp.rewd = max(x,y)*(F+T)
p1.win = binomial_comp(x,y,floor(p1*T),T-floor(p1*T),N1,N2,R1,R2)
p2.win = 1 - p1.win
cur.exp.rewd = floor(p1*T)*x + (T-floor(p1*T))*y + p1.win*x*F + p2.win*y*F
return((max.exp.rewd -cur.exp.rewd )*dbeta(x,R1,N1-R1)*dbeta(y,R2,N2-R2))	
}
	
integrate(function(y) { 
   sapply(y, function(y) {
     integrate(function(x) myfun(x,y), 0, 1)$value
   })
 }, 0, 1)
}
compute.ereward2(80,8000,90,9000,1000,0.9,500)
####################################################################################
#test
####################################################################################
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
i = 0:n1
ans = sum(dbinom(i,n1,p1)*pbinom(ceiling((i+R1)*(n2+N2+1)/(n1+N1+1)-1)-R2,n2,p2))
return(ans)
}

compute.ereward = function(R1,N1,R2,N2,F,p1,T){
myfun <- function(x){
max.exp.rewd = max(x)*(F+T)
p1.win = binomial_comp(x[1],x[2],floor(p1*T),T-floor(p1*T),N1,N2,R1,R2)
p2.win = 1 - p1.win
cur.exp.rewd = floor(p1*T)*x[1] + (T-floor(p1*T))*x[2] + p1.win*x[1]*F + p2.win*x[2]*F
return((max.exp.rewd -cur.exp.rewd )*dbeta(x[1],R1,N1-R1)*dbeta(x[2],R2,N2-R2))
}
adaptIntegrate(myfun,rep(0,2), rep(1,2),tol=0.001)
}
compute.ereward(80,8000,90,8900,1000,0.9,500)

n1 = 1000
n2 = 1000
n_exp = 1000
n_f = 1000000
p = runif(2, 0, 0.1)
p = c(0.0703,0.0649)
r1 = rbinom(1,size=n1, prob=p[1])
r2 = rbinom(1,size=n2, prob=p[2])
print(r1/n1)
print(r2/n2)
for(prob in seq(0.5,0.7,by =0.01))
{
	a = compute.ereward(r1,n1,r2,n2,n_f,prob,n_exp)$integral
	print(a)
}

compute.prbopt(c(r1,r2),c(n1,n2))


compute.ereward(r1,n1,r2,n2,n_f,0.2,n_exp)$integral
compute.ereward(r1,n1,r2,n2,n_f,0.3,n_exp)$integral
compute.ereward(r1,n1,r2,n2,n_f,0.4,n_exp)$integral
compute.ereward(r1,n1,r2,n2,n_f,0.5,n_exp)$integral
compute.ereward(r1,n1,r2,n2,n_f,0.6,n_exp)$integral
compute.ereward(r1,n1,r2,n2,n_f,0.7,n_exp)$integral

power.prop.test(n = 50, p1 = .50, p2 = .75)

qnorm(0.975)

powsize = function(p1,p2,n1,n2,alpha){
	alpha = alpha
	sd = sqrt(p1*(1-p1)/n1+p2*(1-p2)/n2)
	p = (p1*n1+p2*n2)/(n1+n2)
	sd_m = sqrt(p*(1-p)/n1+p*(1-p)/n2)
	q1 = (qnorm(1-alpha/2)*sd_m-(p1-p2))/sd
	q2 = (-qnorm(1-alpha/2)*sd_m-(p1-p2))/sd
	ans = 1-pnorm(q1)+pnorm(q2)
	return(ans)
}

n_ctrl = 1:4999
power = powsize(0.01,0.02,n_ctrl,5000-n_ctrl,0.05)


plot(n_ctrl,power,type="l", col="blue")
lines(rep(which(power==max(power)),100),seq(0,max(power),length.out=100),lty=2)
lines(seq(1,which(power==max(power)),length.out=100),rep(max(power),100),lty=2)


sdcal= function(p1,p2,n1,n2){
	sd = sqrt(p1*(1-p1)/n1+p2*(1-p2)/n2)
	return(sd)
}
n_ctrl = 1:4999
SD = sdcal(0.01,0.02,n_ctrl,5000-n_ctrl)
n_ratio = n_ctrl/(5000-n_ctrl)
plot(n_ctrl,SD,type="l", col="blue")

lines(rep(which(SD==min(SD)),100),seq(0,min(SD),length.out=100),lty=2)
lines(seq(1,which(SD==min(SD)),length.out=100),rep(min(SD),100),lty=2)
text(which(SD==min(SD)),min(SD)+0.01,"(386,  0.01741724)")





a=function(x,n,a,b){
	ans = lbeta(a,b)-lbeta(a+x,b+n-x)+x*(digamma(a+x)-digamma(a+b+n))+(n-x)*(digamma(b+n-x)-digamma(a+b+n))
	return(ans)
}

inf = function(n,a,b){
	x=0:n
	#ans = sum(beta(a+x,b+n-x)*a(x,n,a,b)/(n+1)/beta(1+x,1+n-x)/beta(a,b))
	ans = sum(exp(lbeta(a+x,b+n-x)-lbeta(1+x,1+n-x)-lbeta(a,b)-log(n+1))*a(x,n,a,b))
	return(ans)
}
n_ctrl = 1:199
Lindley_Info = rep(0,199)
for (i in n_ctrl){

j = 200-i
l1 = inf(i,54,846)
l2 = inf(j,70,930)
	
Lindley_Info[i] = l1 + l2
}
plot(n_ctrl,Lindley_Info,type="l", col="blue")
lines(rep(which(Lindley_Info==max(Lindley_Info)),100),seq(0,max(Lindley_Info),length.out=100),lty=2)
lines(seq(1,which(Lindley_Info==max(Lindley_Info)),length.out=100),rep(max(Lindley_Info),100),lty=2)
text(which(Lindley_Info==max(Lindley_Info))+20,max(Lindley_Info),"(150,  0.1012432)")

plot(1:200,a(1:200,200,60,940))


a(floor(300*3/73),200,30,700)
inf(300,30,700)


inf2 = function(n,a,b){
	x=0:n
	ans = sum(beta(a+x,b+n-x)/(n+1)/beta(1+x,1+n-x)/beta(a,b))
	return(ans)
}
inf2(200,400,500)

max_v = 0
max_id =-1
nn = 1000
for(id in 1:nn){
	l1 = inf(id,10,490)
	l2 = inf(nn-id,100,900)
	if((l1+l2)>max_v){
		max_v = l1+l2
		max_id =id
}
}


inf = function(n,a,b){
	x=0:n
	#ans = sum(beta(a+x,b+n-x)*a(x,n,a,b)/(n+1)/beta(1+x,1+n-x)/beta(a,b))
	ans = (exp(lbeta(a+x,b+n-x)-lbeta(1+x,1+n-x)-lbeta(a,b)-log(n+1)))
	return(ans)
}
inf(3,4,6)%*%t(inf(4,4,2))


compute.prbopt3<-function(alpha1,beta1,alpha2,beta2){
		f <- function(x){
			r = x*pbeta(x,alpha2,beta2)-pbeta(x,alpha2+1,beta2)*alpha2/(beta2+alpha2)
			r = r*dbeta(x,alpha1,beta1)
			return (r)
			}
		ans= integrate(f,0,1)$value
	return(ans)
}

for(i in c(0,10,20,40,50,60,80,100)){
	sum = 0
	m = inf(i,4,6)%*%t(inf(100-i,4,2))
	for(j in 0:i)
		for(k in 0:(100-i)){
			a1 = j+4
			b1 = i-j+6
			a2 = k +4
			b2 = 100-i-k+2
			if((a1/b1)>=(a2/b2))
				sum = sum + compute.prbopt3(a1,b1,a2,b2)*m[j+1,k+1]
			else
				sum = sum + compute.prbopt3(a2,b2,a1,b1)*m[j+1,k+1]						
}
print(sum)
}

compute.prbopt3(1,99,2,98)

for(i in c(10,20,30,40,50,60,70,80,90,100)){
	
	print(-compute.prbopt3(1+i*1/100,99+i*99/100,2+(100-i)*2/100,98+(100-i)*98/100)*10000+i*1/100)
}


Test_Period = 1:21
dat = rbind(RPM_SIZE1,RPM_SIZE2,PRO_SIZE1,PRO_SIZE2)
dat = t(dat)
matplot(dat,xlab = "Period",ylab = "Test Power", type = c("l"),pch=1,lwd =3) #plot
legend("top", legend = 1:4, col=1:6, lty=1) # optional legend


binomial_comp = function(p1,p2,T,delta){
i = 0:(T/2)
ans = sum(dbinom(i,T/2,p1)*pbinom(i-delta,T/2,p2))
return(ans)
}

compute.ereward = function(R1,N1,R2,N2,F,T){
	
myfun <- function(x){	
	p = binomial_comp(x[1],x[2],T,R2-R1)
	ans = dbeta(x[1],R1,N1-R1)*dbeta(x[2],R2,N2-R2)*((p*x[1]+(1-p)*x[2])*F + mean(x)*T)
	return(ans)
	}
return(max(R1/N1,R2/N2)*(F+T)-adaptIntegrate(myfun,rep(0,2), rep(1,2),tol =0.001)$integral)
}

x=60
compute.ereward(5,100,6,100,200-x,x)

p1=0.21
p2=0.22
n=4000
delta=13
binomial_comp(p1,p2,n,delta)
pnorm((p1-p2-delta/n*2)/sqrt(p1*(1-p1)+p2*(1-p2))*sqrt(n/2))