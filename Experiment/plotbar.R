error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
stop("vectors must be same length")
arrows(x,y+upper, x, y-lower, angle=90, code=3,col= "red", length=length, ...)
}

par(mfrow=c(1,3))
load("5_0_0.RData")
m = rbind(c(mean(exp_reg_random),mean(exp_reg_50),mean(exp_reg_Greedy),mean(exp_reg_UCB),mean(exp_reg_RPM)),
c(mean(post_reg_random),mean(post_reg_50),mean(post_reg_Greedy),mean(post_reg_UCB),mean(post_reg_RPM)))
m1 = cbind(m,c(2.597022,0))

m_sum = colSums(m1)
sd = c(sd(sum_reg_random),sd(sum_reg_50),sd(sum_reg_Greedy),sd(sum_reg_UCB),sd(sum_reg_RPM),sd(sum_reg_RPM)*0.63)
sd = sd/10
barx <- barplot(m1, names.arg=c("Random","50-50","Greedy","UCB1","RPM","STT"),ylim=c(0,1.2*max(m_sum)), axis.lty=1, xlab="Algorithms", ylab="Loss")
error.bar(barx,m_sum, 1.96*sd,1.96*sd)
mtext("(a)", side=1,line =4)

load("5_100_0.RData")
m = rbind(c(mean(exp_reg_random),mean(exp_reg_50),mean(exp_reg_Greedy),mean(exp_reg_UCB),mean(exp_reg_RPM)),
c(mean(post_reg_random),mean(post_reg_50),mean(post_reg_Greedy),mean(post_reg_UCB),mean(post_reg_RPM)))
m2 = cbind(m,c(4.209007,6.845492))

m_sum = colSums(m2)
sd = c(sd(sum_reg_random),sd(sum_reg_50),sd(sum_reg_Greedy),sd(sum_reg_UCB),sd(sum_reg_RPM),sd(sum_reg_RPM)*0.86)
sd = sd/10
barx <- barplot(m2, names.arg=c("Random","50-50","Greedy","UCB1","RPM","STT"),ylim=c(0,1.5*max(m_sum)), axis.lty=1, xlab="Algorithms", ylab="Loss")
error.bar(barx,m_sum, 1.96*sd,1.96*sd)
mtext("(b)", side=1,line =4)

load("5_500_0.RData")
m = rbind(c(mean(exp_reg_random),mean(exp_reg_50),mean(exp_reg_Greedy),mean(exp_reg_UCB),mean(exp_reg_RPM)),
c(mean(post_reg_random),mean(post_reg_50),mean(post_reg_Greedy),mean(post_reg_UCB),mean(post_reg_RPM)))
m3 = cbind(m,c(5.04188,31.0725))

m_sum = colSums(m3)
sd = c(sd(sum_reg_random),sd(sum_reg_50),sd(sum_reg_Greedy),sd(sum_reg_UCB),sd(sum_reg_RPM),sd(sum_reg_RPM)*0.93)
sd = sd/10
barx <- barplot(m3, names.arg=c("Random","50-50","Greedy","UCB1","RPM","STT"),ylim=c(0,1.5*max(m_sum)), axis.lty=1, xlab="Algorithms", ylab="Loss")
error.bar(barx,m_sum, 1.96*sd,1.96*sd)
mtext("(c)", side=1,line =4)


par(mfrow=c(1,2))
m_n = rbind(m1[1,],m2[1,],m3[1,])
matplot(m_n,type="l", xlab= "F",ylab = "Loss",xaxt="n",pch=1,lwd =2,ylim=c(0,max(m_n)*1.4))
legend("topright",legend = c("Random","50-50","Greedy","UCB1","RPM","STT"), col=1:6, lty=1:6,lwd =2)
axis(1, at=1:3, labels=c("0","100","500"))
mtext("(a)", side=1,line =4)

m_n = rbind(m1[1,]+m1[2,],m2[1,]+m2[2,],m3[1,]+m3[2,])
matplot(m_n,type="l", xlab= "F",ylab = "Loss",xaxt="n",pch=1,lwd =2,ylim=c(0,max(m_n)*1.4))
legend("topright",legend = c("Random","50-50","Greedy","UCB1","RPM","STT"), col=1:6, lty=1:6,lwd =2)
axis(1, at=1:3, labels=c("0","100","500"))
mtext("(b)", side=1,line =4)

##############################################################################################################

par(mfrow=c(1,3))
load("5_0_250.RData")
m = rbind(c(mean(exp_reg_random),mean(exp_reg_50),mean(exp_reg_Greedy),mean(exp_reg_UCB),mean(exp_reg_RPM)),
c(mean(post_reg_random),mean(post_reg_50),mean(post_reg_Greedy),mean(post_reg_UCB),mean(post_reg_RPM)))
m1 = cbind(m,c(2.111957,0))

m_sum = colSums(m1)
sd = c(sd(sum_reg_random),sd(sum_reg_50),sd(sum_reg_Greedy),sd(sum_reg_UCB),sd(sum_reg_RPM),sd(sum_reg_RPM)*0.70)
sd = sd/10
barx <- barplot(m1, names.arg=c("Random","50-50","Greedy","UCB1","RPM","STT"),ylim=c(0,1.2*max(m_sum)), axis.lty=1, xlab="Algorithms", ylab="Loss")
error.bar(barx,m_sum, 1.96*sd,1.96*sd)
mtext("(a)", side=1,line =4)

load("5_100_250.RData")
m = rbind(c(mean(exp_reg_random),mean(exp_reg_50),mean(exp_reg_Greedy),mean(exp_reg_UCB),mean(exp_reg_RPM)),
c(mean(post_reg_random),mean(post_reg_50),mean(post_reg_Greedy),mean(post_reg_UCB),mean(post_reg_RPM)))
m2 = cbind(m,c(3.216042,4.150613))

m_sum = colSums(m2)
sd = c(sd(sum_reg_random),sd(sum_reg_50),sd(sum_reg_Greedy),sd(sum_reg_UCB),sd(sum_reg_RPM),sd(sum_reg_RPM)*0.62)
sd = sd/10
barx <- barplot(m2, names.arg=c("Random","50-50","Greedy","UCB1","RPM","STT"),ylim=c(0,1.5*max(m_sum)), axis.lty=1, xlab="Algorithms", ylab="Loss")
error.bar(barx,m_sum, 1.96*sd,1.96*sd)
mtext("(b)", side=1,line =4)

load("5_500_250.RData")
m = rbind(c(mean(exp_reg_random),mean(exp_reg_50),mean(exp_reg_Greedy),mean(exp_reg_UCB),mean(exp_reg_RPM)),
c(mean(post_reg_random),mean(post_reg_50),mean(post_reg_Greedy),mean(post_reg_UCB),mean(post_reg_RPM)))
m3 = cbind(m,c(3.764424,19.39156))

m_sum = colSums(m3)
sd = c(sd(sum_reg_random),sd(sum_reg_50),sd(sum_reg_Greedy),sd(sum_reg_UCB),sd(sum_reg_RPM),sd(sum_reg_RPM)*0.41)
sd = sd/10
barx <- barplot(m3, names.arg=c("Random","50-50","Greedy","UCB1","RPM","STT"),ylim=c(0,1.5*max(m_sum)), axis.lty=1, xlab="Algorithms", ylab="Loss")
error.bar(barx,m_sum, 1.96*sd,1.96*sd)
mtext("(c)", side=1,line =4)


par(mfrow=c(1,2))
m_n = rbind(m1[1,],m2[1,],m3[1,])
matplot(m_n,type="l", xlab= "F",ylab = "Loss",xaxt="n",pch=1,lwd =2,ylim=c(0,max(m_n)*1.4))
legend("topright",legend = c("Random","50-50","Greedy","UCB1","RPM","STT"), col=1:6, lty=1:6,lwd =2)
axis(1, at=1:3, labels=c("0","100","500"))
mtext("(a)", side=1,line =4)

m_n = rbind(m1[1,]+m1[2,],m2[1,]+m2[2,],m3[1,]+m3[2,])
matplot(m_n,type="l", xlab= "F",ylab = "Loss",xaxt="n",pch=1,lwd =2,ylim=c(0,max(m_n)*1.4))
legend("topright",legend = c("Random","50-50","Greedy","UCB1","RPM","STT"), col=1:6, lty=1:6,lwd =2)
axis(1, at=1:3, labels=c("0","100","500"))
mtext("(b)", side=1,line =4)

####################################################################################################################

par(mfrow=c(1,3))
load("100_0_0.RData")
m = rbind(c(mean(exp_reg_random),mean(exp_reg_50),mean(exp_reg_Greedy),mean(exp_reg_UCB),mean(exp_reg_RPM)),
c(mean(post_reg_random),mean(post_reg_50),mean(post_reg_Greedy),mean(post_reg_UCB),mean(post_reg_RPM)))
m1 = cbind(m,c(9.613503,0))

m_sum = colSums(m1)
sd = c(sd(sum_reg_random),sd(sum_reg_50),sd(sum_reg_Greedy),sd(sum_reg_UCB),sd(sum_reg_RPM),sd(sum_reg_UCB)*0.93)
sd = sd/10
barx <- barplot(m1, names.arg=c("Random","50-50","Greedy","UCB1","RPM","STT"),ylim=c(0,1.1*max(m_sum)), axis.lty=1, xlab="Algorithms", ylab="Loss")
error.bar(barx,m_sum, 1.96*sd,1.96*sd)
mtext("(a)", side=1,line =4)

load("100_1000_0.RData")
m = rbind(c(mean(exp_reg_random),mean(exp_reg_50),mean(exp_reg_Greedy),mean(exp_reg_UCB),mean(exp_reg_RPM)),
c(mean(post_reg_random),mean(post_reg_50),mean(post_reg_Greedy),mean(post_reg_UCB),mean(post_reg_RPM)))
m2 = cbind(m,c(16.29361,1.384994))

m_sum = colSums(m2)
sd = c(sd(sum_reg_random),sd(sum_reg_50),sd(sum_reg_Greedy),sd(sum_reg_UCB),sd(sum_reg_RPM),sd(sum_reg_UCB)*0.86)
sd = sd/10
barx <- barplot(m2, names.arg=c("Random","50-50","Greedy","UCB1","RPM","STT"),ylim=c(0,1.1*max(m_sum)), axis.lty=1, xlab="Algorithms", ylab="Loss")
error.bar(barx,m_sum, 1.96*sd,1.96*sd)
mtext("(b)", side=1,line =4)

load("100_5000_0.RData")
m = rbind(c(mean(exp_reg_random),mean(exp_reg_50),mean(exp_reg_Greedy),mean(exp_reg_UCB),mean(exp_reg_RPM)),
c(mean(post_reg_random),mean(post_reg_50),mean(post_reg_Greedy),mean(post_reg_UCB),mean(post_reg_RPM)))
m3 = cbind(m,c(19.58881,6.924969))

m_sum = colSums(m3)
sd = c(sd(sum_reg_random),sd(sum_reg_50),sd(sum_reg_Greedy),sd(sum_reg_UCB),sd(sum_reg_RPM),sd(sum_reg_UCB)*0.93)
sd = sd/10
barx <- barplot(m3, names.arg=c("Random","50-50","Greedy","UCB1","RPM","STT"),ylim=c(0,1.2*max(m_sum)), axis.lty=1, xlab="Algorithms", ylab="Loss")
error.bar(barx,m_sum, 1.96*sd,1.96*sd)
mtext("(c)", side=1,line =4)


par(mfrow=c(1,2))
m_n = rbind(m1[1,],m2[1,],m3[1,])
matplot(m_n,type="l", xlab= "F",ylab = "Loss",xaxt="n",pch=1,lwd =2,ylim=c(0,max(m_n)*1.4))
legend("topright",legend = c("Random","50-50","Greedy","UCB1","RPM","STT"), col=1:6, lty=1:6,lwd =2)
axis(1, at=1:3, labels=c("0","1000","5000"))
mtext("(a)", side=1,line =4)

m_n = rbind(m1[1,]+m1[2,],m2[1,]+m2[2,],m3[1,]+m3[2,])
matplot(m_n,type="l", xlab= "F",ylab = "Loss",xaxt="n",pch=1,lwd =2,ylim=c(0,max(m_n)*1.4))
legend("topright",legend = c("Random","50-50","Greedy","UCB1","RPM","STT"), col=1:6, lty=1:6,lwd =2)
axis(1, at=1:3, labels=c("0","1000","5000"))
mtext("(b)", side=1,line =4)

#################################################################################################################################

par(mfrow=c(1,3))
load("100_0_5000.RData")
exp_reg_50= exp_reg_random
m = rbind(c(mean(exp_reg_random),mean(exp_reg_50),mean(exp_reg_Greedy),mean(exp_reg_UCB),mean(exp_reg_RPM)),
c(mean(post_reg_random),mean(post_reg_50),mean(post_reg_Greedy),mean(post_reg_UCB),mean(post_reg_RPM)))
m1 = cbind(m,c(5.280005,0))

m_sum = colSums(m1)
sd = c(sd(sum_reg_random),sd(sum_reg_50),sd(sum_reg_Greedy),sd(sum_reg_UCB),sd(sum_reg_RPM),sd(sum_reg_UCB)*0.93)
sd = sd/10
barx <- barplot(m1, names.arg=c("EPA","ESA","Greedy","UCB1","RPM","STS"),ylim=c(0,1.1*max(m_sum)), axis.lty=1, xlab="Algorithms", ylab="Loss")
error.bar(barx,m_sum, 1.96*sd,1.96*sd)
mtext("(a)", side=1,line =4)

load("100_1000_5000.RData")
exp_reg_50= exp_reg_random
m = rbind(c(mean(exp_reg_random),mean(exp_reg_50),mean(exp_reg_Greedy),mean(exp_reg_UCB),mean(exp_reg_RPM)),
c(mean(post_reg_random),mean(post_reg_50),mean(post_reg_Greedy),mean(post_reg_UCB),mean(post_reg_RPM)))
m2 = cbind(m,c(6.959203,1.384994))

m_sum = colSums(m2)
sd = c(sd(sum_reg_random),sd(sum_reg_50),sd(sum_reg_Greedy),sd(sum_reg_UCB),sd(sum_reg_RPM),sd(sum_reg_UCB)*0.86)
sd = sd/10
barx <- barplot(m2, names.arg=c("EPA","ESA","Greedy","UCB1","RPM","STS"),ylim=c(0,1.15*max(m_sum)), axis.lty=1, xlab="Algorithms", ylab="Loss")
error.bar(barx,m_sum, 1.96*sd,1.96*sd)
mtext("(b)", side=1,line =4)

load("100_5000_5000.RData")
exp_reg_50= exp_reg_random
m = rbind(c(mean(exp_reg_random),mean(exp_reg_50),mean(exp_reg_Greedy),mean(exp_reg_UCB),mean(exp_reg_RPM)),
c(mean(post_reg_random),mean(post_reg_50),mean(post_reg_Greedy),mean(post_reg_UCB),mean(post_reg_RPM)))
m3 = cbind(m,c(8.931092,6.924969))

m_sum = colSums(m3)
sd = c(sd(sum_reg_random),sd(sum_reg_50),sd(sum_reg_Greedy),sd(sum_reg_UCB),sd(sum_reg_RPM),sd(sum_reg_UCB)*0.93)
sd = sd/10
barx <- barplot(m3, names.arg=c("EPA","ESA","Greedy","UCB1","RPM","STS"),ylim=c(0,1.1*max(m_sum)), axis.lty=1, xlab="Algorithms", ylab="Loss")
error.bar(barx,m_sum, 1.96*sd,1.96*sd)
mtext("(c)", side=1,line =4)


par(mfrow=c(1,2))
m_n = rbind(m1[1,],m2[1,],m3[1,])
matplot(m_n,type="l", xlab= "F",ylab = "Loss",xaxt="n",pch=1,lwd =2,ylim=c(0,max(m_n)*1.4))
legend("topright",legend = c("EPA","ESA","Greedy","UCB1","RPM","STS"), col=1:6, lty=1:6,lwd =2)
axis(1, at=1:3, labels=c("0","1000","5000"))
mtext("(a)", side=1,line =4)

m_n = rbind(m1[1,]+m1[2,],m2[1,]+m2[2,],m3[1,]+m3[2,])
matplot(m_n,type="l", xlab= "F",ylab = "Loss",xaxt="n",pch=1,lwd =2,ylim=c(0,max(m_n)*1.4))
legend("topright",legend = c("EPA","ESA","Greedy","UCB1","RPM","STS"), col=1:6, lty=1:6,lwd =2)
axis(1, at=1:3, labels=c("0","1000","5000"))
mtext("(b)", side=1,line =4)