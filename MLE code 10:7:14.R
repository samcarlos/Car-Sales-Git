
library(dlm)

##MLE

current.month="2014-09-01"
cars=read.csv(paste("/Users/sweiss/Google Drive/by the numbers folder/merged.data", current.month,".csv",sep=""))

data=cars[,-c(1:3)]
data=data[,-which(colnames(data)=="dates")]
data=data[,-which(colnames(data)=="Number.of.Days.sold")]
data=data[,-which(colnames(data)=="new.dates..1.")]

data=data[,-which(colSums(is.na(data))>0)]
data=data[,-which(data[109,]==0)]
data=data[,-which(colSums(data==0)>0)]
data=data/cars$Number.of.Days.sold

build.mod=function(parm){
  mod=dlmModPoly()+dlmModSeas(12)
  mod$m0[1]=y[1]
  mod$V=parm[1]
  diag(mod$W)[1:3]=parm[2:4]
  return(mod)
}

parm.values=vector()
for(i in 1:dim(data)[2]){
  y <- ts((data[,i]), frequency = 1) 
  fit <- dlmMLE(y, parm =rep(1,4), build =build.mod, lower=rep(10e-10,4))
  parm.values=rbind(parm.values,fit$par)
}

len.dat=dim(data)[1]

statesGrowth=matrix(0,len.dat,dim(parm.values)[1])
statesSeason=matrix(0,len.dat,dim(parm.values)[1])
statesLevel=matrix(0,len.dat,dim(parm.values)[1])

n.ahead=12
predictLevel=matrix(0,n.ahead,dim(parm.values)[1])
predictGrowth=matrix(0,n.ahead,dim(parm.values)[1])
predictSeason=matrix(0,n.ahead,dim(parm.values)[1])

sdForLevel=matrix(0,n.ahead,dim(parm.values)[1])
sdForMu=matrix(0,n.ahead,dim(parm.values)[1])
sdForSeas=matrix(0,n.ahead,dim(parm.values)[1])


for(i in 1:dim(parm.values)[1]){
  mod.final=build.mod(parm.values[i,])
  y <- ts((data[,i]), frequency = 1) 
  fit.filter <- dlmFilter(y, mod.final)
  modSmooth=dlmSmooth(fit.filter)
  
  statesLevel[,i]=modSmooth$s[-1,1]
  statesGrowth[,i]=modSmooth$s[-1,2]
  statesSeason[,i]=modSmooth$s[-1,3]
  
  forecast=dlmForecast(fit.filter,nAhead=12)
  predictLevel[,i]=forecast$a[,1]
  predictGrowth[,i]=forecast$a[,2]
  predictSeason[,i]=forecast$a[,3]
  
  
  sdForLevel[,i]=sapply(forecast$R,function(x)sqrt(x[1,1]))
  sdForMu[,i]=sapply(forecast$R,function(x)sqrt(x[2,2]))
  sdForSeas[,i]=sapply(forecast$R,function(x)sqrt(x[3,3]))
  
  
}
colnames(statesGrowth)=colnames(data)
colnames(statesSeason)=colnames(data)
colnames(statesLevel)=colnames(data)

colnames(predictLevel)=colnames(data)
colnames(predictGrowth)=colnames(data)
colnames(predictSeason)=colnames(data)


colnames(sdForLevel)=colnames(data)
colnames(sdForMu)=colnames(data)
colnames(sdForSeas)=colnames(data)

puMu=predictGrowth+qnorm(0.95,sd=sdForMu)
plMu=predictGrowth+qnorm(0.05,sd=sdForMu)

puLevel=predictLevel+qnorm(0.95,sd=sdForLevel)
plLevel=predictLevel+qnorm(0.05,sd=sdForLevel)

puSeas=predictSeason+qnorm(0.95,sd=sdForSeas)
plSeas=predictSeason+qnorm(0.05,sd=sdForSeas)


save(statesGrowth,statesSeason,statesLevel,predictLevel,predictGrowth,predictSeason,puMu,puLevel,puSeas,plMu,plLevel,plSeas,data, file="/Users/sweiss/Desktop/carsales/data/out.rdata")



predict.next.month=t(as.matrix(predictLevel[1,]+predictSeason[1,]))
rownames(predict.next.month)="Predicted Values for 10/14"
stargazer(t(predict.next.month), type="html")

log.diff.expected=as.matrix(log(data[111,]/predict.next.month))
deviance=as.matrix((abs(data[111,]-predict.next.month))/(sdForSeas[1,]+sdForLevel[1,]))

pub.table=rbind(predict.next.month, (data[111,]),log.diff.expected, deviance)
pub.table=t(pub.table)
colnames(pub.table)=c("Predicted Values 9/14", "Actual Values 9/14", "log(Predicted/Actual)", "Deviance")
library(stargazer)
stargazer(pub.table, type="html")

barplot(pub.table[,3:4])
pub.table.2=pub.table[names(sort(pub.table[,3])),]
par(mfrow=c(2,1))
library(ggplot2)
barplot(pub.table.2[,3],xaxt="n", main="log(Observed/Expected) for Car Sales 09/14")
text( x=1.182759*1:29, y=0,names(sort(pub.table[,3])),  srt=45)
barplot(pub.table.2[,4],xaxt="n", main="Deviance Given Expected Standard Deviation Forecast Car Sales 09/14")
text( x=1.182759*1:29, y=1,names(sort(pub.table[,3])),  srt=45)
