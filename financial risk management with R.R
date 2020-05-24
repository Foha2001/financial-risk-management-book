
############Financial Risk management with R######################
###################book####################################
#################courseraa########################################
library(quantmod)
wilsh<-getSymbols("GOLDPMGBD228NLBM",src="FRED",auto.assign=FALSE)
wilsh <- na.omit(wilsh)
wilsh <- wilsh["1979-12-31/2017-12-31"]
names(wilsh) <- "TR"
head(wilsh,3)
tail(wilsh,3)
logret <- diff(log(wilsh)) 
head(logret,3) 
logret <- diff(log(wilsh))[-1] 
round(head(logret,3),6)
ret <- exp(logret) - 1
round(head(ret,3),6) 
round(tail(ret,3),6) 
logret.w <- apply.weekly(logret,sum)
logret.m <- apply.monthly(logret,sum)
logret.q <- apply.quarterly(logret,sum)
logret.y <- apply.yearly(logret,sum)
ret.w <- exp(logret.w)-1
ret.m <- exp(logret.m)-1
ret.q <- exp(logret.q)-1
ret.y <- exp(logret.y)-1
round(head(logret.w,3),6) 
round(head(logret.m,3),6) 
round(head(ret.q,3),6) 
round(tail(ret.y,3),6)
################################QUIZ1#################################
library(quantmod)
DEXJPUS=getSymbols("DEXJPUS", src="FRED",auto.assign=FALSE)
DEXJPUS=na.omit(DEXJPUS)
DEXJPUS<- DEXJPUS["1979-12-31/2017-12-31"]
DEXJPUS = 1/DEXJPUS
logreturn=diff(log(DEXJPUS))[-1]
return=exp(logreturn)-1
logreturn.w=apply.weekly(logreturn,sum)
logreturn.m=apply.monthly(logreturn,sum)
logreturn.q=apply.quarterly(logreturn,sum)
logreturn.y=apply.yearly(logreturn,sum)
return.w=exp(logreturn.w)-1
return.m=exp(logreturn.m)-1
return.q=exp(logreturn.q)-1
return.y=exp(logreturn.y)-1
round(head(logreturn,3),6)
round(head(return.m,3),6)
round(tail(logreturn.q,3),6)
round(tail(return.y,3),6)
######################quiz2##################################
library(quantmod)
DEXUSUK=getSymbols("DEXUSUK", src="FRED",auto.assign=FALSE)
DEXUSUK=na.omit(DEXUSUK)
DEXUSUK<-DEXUSUK["1979-12-31/2017-12-31"]
logreturn=diff(log(DEXUSUK))[-1]
return=exp(logreturn)-1
logreturn.w=apply.weekly(logreturn,sum)
logreturn.m=apply.monthly(logreturn,sum)
logreturn.q=apply.quarterly(logreturn,sum)
logreturn.y=apply.yearly(logreturn,sum)
return.w=exp(logreturn.w)-1
return.m=exp(logreturn.m)-1
return.q=exp(logreturn.q)-1
return.y=exp(logreturn.y)-1
round(head(logreturn),6) 
round(head(return.m),6)  
round(tail(logreturn.q),6)   
round(tail(return.y),6)   
########################quiz3##################################
DEXSZUS=getSymbols("DEXSZUS", src="FRED",auto.assign=FALSE)
DEXSZUS=na.omit(DEXSZUS)
DEXSZUS=DEXSZUS["1979-12-31/2017-12-31"]
DEXSZUS=1/DEXSZUS
logreturn=diff(log(DEXSZUS))[-1]
return=exp(logreturn)-1
logreturn.w=apply.weekly(logreturn,sum)
logreturn.m=apply.monthly(logreturn,sum)
logreturn.q=apply.quarterly(logreturn,sum)
logreturn.y=apply.yearly(logreturn,sum)
return.w=exp(logreturn.w)-1
return.m=exp(logreturn.m)-1
return.q=exp(logreturn.q)-1
return.y=exp(logreturn.y)-1
round(head(logreturn),6)
round(head(return.m),6)
round(tail(logreturn.q),6)
round(tail(return.y),6)
########################quiz4##################################
DEXUSAL=getSymbols("DEXUSAL", src="FRED",auto.assign=FALSE)
DEXUSAL=na.omit(DEXUSAL)
DEXUSAL=DEXUSAL["1979-12-31/2017-12-31"]
logreturn=diff(log(DEXUSAL))[-1]
return=exp(logreturn)-1
logreturn.w=apply.weekly(logreturn,sum)
logreturn.m=apply.monthly(logreturn,sum)
logreturn.q=apply.quarterly(logreturn,sum)
logreturn.y=apply.yearly(logreturn,sum)
return.w=exp(logreturn.w)-1
return.m=exp(logreturn.m)-1
return.q=exp(logreturn.q)-1
return.y=exp(logreturn.y)-1
round(head(logreturn),6)
round(head(return.m),6)
round(tail(logreturn.q),6)
round(tail(return.y),6)
##################week 2###################################
library(quantmod)
wilsh<-getSymbols("GOLDPMGBD228NLBM",src="FRED",auto.assign=FALSE)
wilsh <- na.omit(wilsh)
wilsh <- wilsh["1979-12-31/2017-12-31"]
names(wilsh) <- "TR"
logret <- diff(log(wilsh))[-1]
round(mean(logret),8)
round(sd(logret),6)
#############################exercice 6##############################
mu <- mean(logret)
sig <- sd(logret)
var <- qnorm(0.05,mu,sig)
HFvar <- 1000 * ( exp(var)-1 ) 
round(var,6)
round(HFvar,1)
###############################exercice 7############################
es <- mu-sig*dnorm(qnorm(0.05,0,1),0,1)/0.05
HFvar <- 1000 * ( exp(es)-1 ) 
round(es,6)
round(HFvar,1)
###########################exercice 8#############################
set.seed(123789)
rvec <- rnorm(100000,mu,sig)
VaR <- quantile(rvec,0.05)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)
set.seed(123789)
rvec <- sample(as.vector(logret),100000,replace=TRUE)
VaR <- quantile(rvec,0.05)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)
###############quiz1 week  2#################################
DEXJPUS=getSymbols("DEXJPUS", src="FRED", auto.assign = FALSE)
DEXJPUS=na.omit(DEXJPUS)
DEXJPUS=DEXJPUS["1979-12-31/2017-12-31"]
DEXJPUS=1/DEXJPUS
logreturn=diff(log(DEXJPUS))[-1]
return=exp(logreturn)-1
mea=mean(logreturn)
str=sd(logreturn)
VaR=qnorm(0.01,mean(logreturn),sd(logreturn))
round(mean(logreturn),6)
round(sd(logreturn),6)
round(VaR,6)

ES <- mean(logreturn)-sd(logreturn)*dnorm(qnorm(0.01,mea,str),0,1)/0.01


round(ES,6)
set.seed(123789)
rvec <- rnorm(100000,mean(logreturn),sd(logreturn))
VaR <- quantile(rvec,0.01)
ES <- mean(rvec[rvec<VaR])     incorrect
round(VaR,6)
round(ES,6)
set.seed(123789)
rvec <- sample(as.vector(logreturn),100000,replace=TRUE)
VaR <- quantile(rvec,0.01)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)
HFES <- 1000 * ( exp(ES)-1 ) 
round(HFES,2)
###############quiz2 week  2#################################
library(quantmod)
DEXUSUK=getSymbols("DEXUSUK", src="FRED", auto.assign = FALSE)
DEXUSUK=na.omit(DEXUSUK)
DEXUSUK=DEXUSUK["1979-12-31/2017-12-31"]
logreturn=diff(log(DEXUSUK))[-1]
return=exp(logreturn)-1
library(moments)
mea=mean(logreturn)      incorrect 
stan=sd(logreturn)
round(mean(logreturn),6)   
round(sd(logreturn),6)  
VaR=qnorm(0.01,mea,stan)
round(VaR,6)
ES <- mean(logreturn)-sd(logreturn)*dnorm(qnorm(0.01, 0,1),0,1)/0.01  TRY THIS     
round(ES,6)
set.seed(123789)
rvec <- rnorm(100000,mea,stan)
Var <- quantile(rvec,0.01)
ES <- mean(rvec[rvec<Var])
round(Var,6)
round(ES,6)
#set.seed(123789)
rvec <- sample(as.vector(logreturn),100000,replace=TRUE)
VaR <- quantile(rvec,0.01)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)
HFES <- 1000*(exp(ES)-1) 
round(HFES,2)
###############quiz3 week  2#################################
library(quantmod)
DEXSZUS=getSymbols("DEXSZUS", src="FRED", auto.assign = FALSE)
DEXSZUS=na.omit(DEXSZUS)
DEXSZUS=DEXSZUS["1979-12-31/2017-12-31"]
logreturn=diff(log(DEXSZUS))[-1]
return=exp(logreturn)-1
library(moments)
moyenne=mean(logreturn)       
ecart=sd(logreturn)
round(mean(logreturn),6)
round(sd(logreturn),6)
VaR=qnorm(0.01,moyenne,ecart)
round(VaR,6)
ES <- mean(logreturn)-sd(logreturn)*dnorm(qnorm(0.01,0,1),0,1)/0.01      
round(ES,6)
set.seed(123789)
rvec <- rnorm(100000,moyenne,ecart)
Var <- quantile(rvec,0.01)
es <- mean(rvec[rvec<Var])   incorrect         
round(Var,6)
round(es,6)
-------------
set.seed(123789)
rvec1 <- sample(as.vector(logreturn),100000,replace=TRUE)
VAR <- quantile(rvec1,0.01)
ES1 <- mean(rvec1[rvec1<VAR])     
round(VAR,6)
round(ES1,6)
-----------
HFES <- 1000*(exp(ES1)-1) 
round(HFES,2)
###############quiz4 week  2#################################
DEXUSAL=getSymbols("DEXUSAL", src="FRED", auto.assign = FALSE)
DEXUSAL=na.omit(DEXUSAL)
DEXUSAL=DEXUSAL["1979-12-31/2017-12-31"]
logreturn=diff(log(DEXUSAL))[-1]
return=exp(logreturn)-1
mea=mean(logreturn)       
stan=sd(logreturn)
round(mea,6)
round(stan,6)
VaR=qnorm(0.01,mea,stan)      
round(VaR,6)
ES <- mea-stan*dnorm(qnorm(0.01,mea,stan),0,1)/0.01   
round(ES,6)
set.seed(123789)
rvec <- rnorm(100000,mea,stan)
VaR <- quantile(rvec,0.01)      
ES <- mean(rvec[rvec<VaR])      
round(VaR,6)
round(ES,6)
set.seed(123789)
rvec <- sample(as.vector(logreturn),100000,replace=TRUE)
VaR <- quantile(rvec,0.01)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)
HFES <- 1000 * ( exp(ES)-1 ) 
round(HFES,2)

####################week 3 exercice 9###############################
library(quantmod)
wilsh<-getSymbols("GOLDPMGBD228NLBM",src="FRED",auto.assign=FALSE)
wilsh <- na.omit(wilsh)
wilsh <- wilsh["1979-12-31/2017-12-31"]
names(wilsh) <- "TR"
logret <- diff(log(wilsh))[-1]
library(moments)
rvec <- as.vector(logret)
round(skewness(rvec),2)
round(kurtosis(rvec),2)
rvec <- as.vector(logret)
jarque.test(rvec)
###############WEEK 3 EXERCICE 10#####################
library(quantmod)
getSymbols("GOLDPMGBD228NLBM",src="FRED")
wilsh <- na.omit(GOLDPMGBD228NLBM)
wilsh <- wilsh["1979-12-31/2017-12-31"]
names(wilsh) <- "TR"
logret <- diff(log(wilsh))[-1]
library(MASS)
rvec <- as.vector(logret)
t.fit <- fitdistr(rvec,"t")
round(t.fit$estimate,6)
alpha <- 0.05
set.seed(123789)
library(metRology)
rvec <- rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)
####################WEEK 3 EXERCICE 11##################################
library(quantmod)
getSymbols("GOLDPMGBD228NLBM",src="FRED")
wilsh <- na.omit(GOLDPMGBD228NLBM)
wilsh <- wilsh["1979-12-31/2017-12-31"]
names(wilsh) <- "TR"
logret <- diff(log(wilsh))[-1]
library(MASS)
rvec <- as.vector(logret)
t.fit <- fitdistr(rvec,"t")
round(t.fit$estimate,6)
library(metRology)
alpha <- 0.05
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10) {
  rvec <- rvec+rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
}
VaR <- quantile(rvec,alpha)  #incorrect
ES <- mean(rvec[rvec<VaR])    #incorrect 
round(VaR,6)
round(ES,6)
alpha <- 0.05
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10) {
  rvec <- rvec+ sample(as.vector(logret),100000,replace=TRUE)
}
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)

alpha <- 0.05
set.seed(123789)
rdat <- as.vector(logret)
rvec <- rep(0,100000)
posn <- seq(from=1,to=length(rdat)-9,by=1)
rpos <- sample(posn,100000,replace=TRUE)
for (i in 1:10) {
  rvec <- rvec+ rdat[rpos]
  rpos <- rpos+1
}
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)
#########################week 3 QUIZ 1##################################
library(quantmod)
data=getSymbols("DEXJPUS", src="FRED", auto.assign = FALSE)
data=na.omit(data)
data=data["1979-12-31/2017-12-31"]
data=1/data
logreturn=diff(log(data))[-1]
library(moments)
round(skewness(logreturn),2)
round(kurtosis(logreturn),2)
nor=as.vector(logreturn)
jarque.test(nor)

library(MASS)
rvec=as.vector(logreturn)
tfit=fitdistr(rvec,"t")
round(tfit$estimate[1],6)
round(tfit$estimate[2],6)
round(tfit$estimate[3],6)
alpha=0.01
set.seed(123789)
library(metRology)
rvec=rt.scaled(100000,mean=tfit$estimate[1],sd=tfit$estimate[2],df=tfit$estimate[3])
VaR=quantile(rvec,0.01)
ES=mean(rvec[rvec<VaR])
round(ES,6)
rvec=rep(0,100000)
for (i in 1:10 ) {
  rvec=rvec+sample(as.vector(logreturn),100000, replace=TRUE)
  }
VaR=quantile(rvec,0.01)
ES=mean(rvec[rvec<VaR])
round(ES,6)
rvec=rep(0,100000)
for (i in 1:10 ) {
  rvec=rvec+rt.scaled(100000,mean=tfit$estimate[1],sd=tfit$estimate[2],df=tfit$estimate[3])
}
VaR=quantile(rvec,0.01)
ES=mean(rvec[rvec<VaR])
round(ES,6)
rvec=rep(0,100000)
rdat=as.vector(logreturn)
posn=seq(from=1, to=length(rdat)-9,by=1)
rpos=sample(posn,100000,replace=TRUE)
for (i in 1:10 ) {
  rvec=rvec+rdat[rpos]
rpos=rpos+1
}
VaR=quantile(rvec,0.01)
ES=mean(rvec[rvec<VaR])
round(ES,6)
#########################week 3 QUIZ 2##################################
library(quantmod)
data=getSymbols("DEXUSUK", src = "FRED", auto.assign = FALSE)
data=na.omit(data)
data=data["1979-12-31/2017-12-31"]
logreturn=diff(log(data))[-1]
library(moments)
round(skewness(logreturn),2)     #INCORRECT
round(kurtosis(logreturn),2)
vector=as.vector(logreturn)
jarque.test(vector)                   ###   INCORRECT
library(MASS)
fiting=fitdistr(vector,"t")
round(fiting$estimate[1],6)
round(fiting$estimate[2],6)
round(fiting$estimate[3],6)
library(metRology)
rvec=rt.scaled(100000,mean=fiting$estimate[1],sd=fiting$estimate[2],df=fiting$estimate[3])
VaR=quantile(rvec,0.01)
ES=mean(rvec[rvec<VaR])
round(ES,6)
set.seed(123789)
rvec=rep(0,100000)
for (i in 1:10) {
  rvec=rvec+rt.scaled(100000,mean=fiting$estimate[1],sd=fiting$estimate[2],df=fiting$estimate[3])
}                              #####INCORRECT 
VaR=quantile(rvec,0.01)
ES=mean(rvec[rvec<VaR])
round(ES,6)
rvec=rep(0,100000)
for (i in 1:10) {
  rvec=rvec+sample(as.vector(logreturn),100000,replace=TRUE)
}
VaR=quantile(rvec,0.01)
ES=mean(rvec[rvec<VaR])
round(ES,6)
rvec=rep(0,100000)
rdat=as.vector(logreturn)
posn=seq(from=1, to=length(rdat)-9,by=1)
rpos=sample(posn,100000,replace=TRUE)
for (i in 1:10 ) {
  rvec=rvec+rdat[rpos]
  rpos=rpos+1
}
VaR=quantile(rvec,0.01)
ES=mean(rvec[rvec<VaR])
round(ES,6)
#########################week3 QUIZ 4##################################
data=getSymbols("DEXUSAL", src = "FRED", auto.assign = FALSE)
data=na.omit(data)
data=data["1979-12-31/2017-12-31"]
logreturn=diff(log(data))[-1]
library(moments)
round(skewness(logreturn),2)
round(kurtosis(logreturn),2)
v=as.vector(logreturn)
jarque.test(v)
library(MASS)
fiting=fitdistr(v,"t")
round(fiting$estimate[1],6)
round(fiting$estimate[2],6)
round(fiting$estimate[3],6)
library(metRology)
set.seed(123789)
rvec=rt.scaled(100000,mean=fiting$estimate[1],sd=fiting$estimate[2],df=fiting$estimate[3])
VaR=quantile(rvec,0.01)
ES=mean(rvec[rvec<VaR])
round(ES,6)
set.seed(123789)
rvec=rep(0,100000)
for (i in 1:10) {
  rvec=rvec+rt.scaled(100000,mean=fiting$estimate[1],sd=fiting$estimate[2],df=fiting$estimate[3])
}
VaR=quantile(rvec,0.01)
ES=mean(rvec[rvec<VaR])
round(ES,6)
rvec=rep(0,100000)
for (i in 1:10) {
  rvec=rvec+sample(as.vector(logreturn),100000,replace=TRUE)
}
VaR=quantile(rvec,0.01)
ES=mean(rvec[rvec<VaR])
round(ES,6)
rvec=rep(0,100000)
rdat=as.vector(logreturn)
posn=seq(from=1, to=length(rdat)-9,by=1)
rpos=sample(posn,100000,replace=TRUE)
for (i in 1:10 ) {
  rvec=rvec+rdat[rpos]
  rpos=rpos+1
}
VaR=quantile(rvec,0.01)
ES=mean(rvec[rvec<VaR])
round(ES,6)
##################WEEK 4 EXERCICE 2#############################
getSymbols("GOLDPMGBD228NLBM",src="FRED")
wilsh <- na.omit(GOLDPMGBD228NLBM)
wilsh <- wilsh["1979-12-31/2017-12-31"]
names(wilsh) <- "TR"
logret <- diff(log(wilsh))[-1] 
acf(logret)
acf( abs(logret) ) 
library(rugarch) 
uspec <- ugarchspec( variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                     distribution.model = "std") 
fit.garch <- ugarchfit(spec = uspec, data = logret[,1]) 
save1 <- cbind( logret[,1], fit.garch@fit$sigma, fit.garch@fit$z ) 
names(save1) <- c("logret", "s", "z") 
                  
fit.garch
zz=save1$z
acf(abs(zz))
######################WEEK 4 EXERCICE 13###################################
library(quantmod)
getSymbols("GOLDPMGBD228NLBM",src="FRED")
wilsh <- na.omit(GOLDPMGBD228NLBM)
wilsh <- wilsh["1979-12-31/1987-10-19"]
names(wilsh) <- "TR"
logret <- diff(log(wilsh))[-1]
library(rugarch)
uspec <- ugarchspec( variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                     distribution.model = "std")
fit.garch <- ugarchfit(spec = uspec, data = logret[,1])
save1 <- cbind( logret[,1], fit.garch@fit$sigma, fit.garch@fit$z )
names(save1) <- c("logret","s","z")
set.seed(123789) #set seed value
boot.garch <- ugarchboot(fit.garch,method=c("Partial","full")[1], sampling="raw", 
      n.ahead=1,n.bootpred=100000, solver="solnp")
rvec <- boot.garch@fseries
VaR <- quantile(rvec,0.05)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)
##################week4 QUIZ1################################
library(quantmod)
base=na.omit(DEXJPUS)
names(base)=("price")
base=base["1979-12-31/2017-12-31"]
base=1/base
return=diff(log(base))[-1]
acf(return)
acf(abs(return))
library(rugarch)
garch.t=ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),
                   mean.model = list(armaOrder=c(0,0), include.mean=TRUE),
                   distribution.model = "std")
garch.t
fitcharch.t=ugarchfit(spec = garch.t, data=return)
fitcharch.t
set.seed(123789)
boot.garch=ugarchboot(fitcharch.t,method = "Partial",
                      sampling="raw", n.ahead=1,
                      n.bootpred=100000,solver="solnp")
rvec=boot.garch@fseries
VaR=quantile(rvec,0.05)
ES=mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)
