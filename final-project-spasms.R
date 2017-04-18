library(survival)

spasms <- read.csv("./raw-project-data.csv",header=T)
colnames(spasms)[1] <- "Tstart"

#ag model
cox.ag <- coxph(Surv(Tstart,Tstop,Status) ~ WithinDay + Treatment + WithinDay*Treatment + Driving + Sleep + Temperature + 
        Barometer + cluster(Date),data=spasms) 
summary(cox.ag)
res.ag <- cox.zph(cox.ag)
plot(res.ag)
#pwp model
coxph(Surv(Tstart,Tstop,Status) ~ WithinDay + Treatment + Driving + Sleep + Temperature + 
        Barometer + Humidity + cluster(Date) + strata(Enum),data=spasms)

#poisson regression (univariate)
spasms.total <- matrix(0,nrow=52,ncol=6)
colnames(spasms.total) <- c("Count","Date","WithinDay","Treatment","Sleep","Driving")
counter <- 0
j <- 1
for(i in 1:nrow(spasms)) {
  counter <- counter + 1
  if(spasms$Status[i] == 0) {  
    spasms.total[j,] <- c(counter,spasms$Date[i],spasms$WithinDay[i],spasms$Treatment[i],spasms$Sleep[i],spasms$Driving[i])
    counter <- 0
    j <- j+1
  }
}
spasms.total <- as.data.frame(spasms.total)
pois.mod <- glm(Count~WithinDay+Treatment+Sleep+Driving,family=poisson,data=spasms.total)
summary(pois.mod)
exp(glm(Count~WithinDay+Treatment,family=poisson,data=spasms.total)$coefficients)
exp(pois.mod$coefficients)
#first time (univariate)
spasms.first <- spasms[which(spasms$Tstart==0),]

survObj <- with(spasms.first,Surv(Tstop,Status==1))
unv <- coxph(survObj ~ WithinDay + Treatment + Driving + Sleep + Temperature + 
               Barometer + Humidity,data=spasms.first)
summary(unv)
km.as.one <- survfit(survObj ~ 1, data = spasms.first, conf.type = "log-log")
km.by.trt <- survfit(survObj ~ Treatment, data = spasms.first, conf.type = "log-log")
plot(km.by.trt)
plot(km.as.one)

#plots

no.zeros <- spasms[which(spasms$Tstart != 0),]
plot(density(no.zeros[which(no.zeros$Treatment==0),1]),xlim=c(0,360),main="Spasm Times",xlab="Time",lwd=3,col="firebrick")
lines(density(no.zeros[which(no.zeros$Treatment==1),1]),lwd=3,col="cornflowerblue")
legend("topleft",col=c("firebrick","cornflowerblue"),lty=1,legend=c("Placebo","Treatment"),lwd=3)

plot(density(no.zeros[which(no.zeros$Treatment==0),3]),xlim=c(0,80),main="Gap Times",xlab="Time",lwd=3,col="firebrick")
lines(density(no.zeros[which(no.zeros$Treatment==1),3]),lwd=3,col="cornflowerblue")
legend("topright",col=c("firebrick","cornflowerblue"),lty=1,legend=c("Placebo","Treatment"),lwd=3)
