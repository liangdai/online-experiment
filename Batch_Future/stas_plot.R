Test_Period = 1:51
dat = rbind(RDM_Power,RPM_Power,PWR_Power,CI_Power,LIN_Power)
dat = t(dat)
matplot(dat,xlab = "Period",ylab = "Test Power", type = c("l"),pch=1,lwd =3) #plot
legend("top", legend = c("RDM","RPM","PWR","CI","LIN"), col=1:6, lty=1) # optional legend

Test_Period = 1:51
dat = rbind(RDM_CI,RPM_CI,PWR_CI,CI_CI,LIN_CI)
dat = t(dat)
matplot(dat,xlab = "Period",ylab = "Test Power", type = c("l"),pch=1,lwd =3) #plot
legend("top", legend = c("RDM","RPM","PWR","CI","UCB1","LIN"), col=1:5, lty=1) # optional legend

Test_Period = 1:51
dat = rbind(RDM_Winprob,RPM_Winprob,PWR_Winprob,CI_Winprob,LIN_Winprob)
dat = t(dat)
matplot(dat,xlab = "Period",ylab = "Win prob", type = c("l"),pch=1,lwd =3) #plot
legend("top", legend = c("RDM","RPM","PWR","CI","UCB1","LIN"), col=1:5, lty=1) # optional legend


Test_Period = 10:51
dat = rbind(RDM_Power,RPM_Power,PWR_Power,CI_Power,LIN_Power)
dat = t(dat)
dat = dat[Test_Period,]
matplot(Test_Period,dat,xlab = "Period",ylab = "Test Power", type = c("l"),pch=1,lwd =3) #plot
legend("toplef", legend = c("RDM","RPM","PWR","CI","LIN"), col=1:6, lty=1) # optional legend

Test_Period = 10:51
dat = rbind(RDM_CI,RPM_CI,PWR_CI,CI_CI,LIN_CI)
dat = t(dat)
dat = dat[Test_Period,]
matplot(Test_Period,dat,xlab = "Period",ylab = "SD", type = c("l"),pch=1,lwd =3) #plot
legend("topright", legend = c("RDM","RPM","PWR","CI","LIN"), col=1:5, lty=1) # optional legend

Test_Period = 10:51
dat = rbind(RDM_Winprob,RPM_Winprob,PWR_Winprob,CI_Winprob,LIN_Winprob)
dat = t(dat)
dat = dat[Test_Period,]
matplot(Test_Period,dat,xlab = "Period",ylab = "Win prob", type = c("l"),pch=1,lwd =3) #plot
legend("toplef", legend = c("RDM","RPM","PWR","CI","LIN"), col=1:5, lty=1) # optional legend



