binomial_comp = function(p1,p2,n1,n2,N1=0,N2=0,R1=0,R2=0){
i = 0:n1
ans = sum(dbinom(i,n1,p1)*pbinom(((i+R1)*(n2+N2)/(n1+N1))-R2,n2,p2))
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


T=seq(0,4000,by=500)
Optimal.P.ctrl = rep(NA,length(T))
for(i in 1:length(T)){
Optimal.P.ctrl[i] = prob4(18,102,16,102,T[i],100)
print(i)
}


dat = rbind(Optimal.P.ctrl,Optimal.P.ctrl.2)
dat = t(dat)
matplot(T,dat,xlab = "n(2)",ylab = "Optimal P_ctrl", type = c("l"),pch=1,lwd =3) #plot
legend("topright", legend = c("Binomial","Normal"), col=1:2, lty=1) # optional legend




