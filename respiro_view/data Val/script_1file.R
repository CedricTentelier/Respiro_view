library(lubridate)
setwd("C:/Users/Cedric/Desktop/respiro_view/data Val/raw files")
data1<-read.table("11-01-18B-ch3.txt", skip=39, sep=";", dec=",")
colnames(data1)<-c("Date","Time","Logtime","Oxy","Phase","Amp","Temp")

filter<-function(data,debut,nclose,nopen,nch,ncouple,np,nm) {
  len<-length(data$Logtime)
  iter<-c(1:len)	
  databis<- subset(data, iter>(debut-1) & (((iter-debut)/(nopen+nclose)-floor((iter-debut)/(nopen+nclose)))+0.00001)<(nclose/(nclose+nopen)) )		
  nsession<-(round((length(databis$Logtime))/(nclose)))
  session<-rep(1:nsession,each=(nclose))
  databis<-subset(databis,c(1:length(databis$Logtime))<=length(session))
  plot(data$Logtime,data$Oxy,cex=0.5)
  points(databis$Logtime,databis$Oxy,col="red",cex=0.5)	
  chambre<-rep(nch,length(databis$Logtime))
  couple<-rep(ncouple,length(databis$Logtime))
  father<-rep(np,length(databis$Logtime))
  mother<-rep(nm,length(databis$Logtime))
  date<-databis$Date
  time<-databis$Time
  logtime<-databis$Logtime
  oxy<-databis$Oxy
  phase<-databis$Phase
  amp<-databis$Amp
  temp<-databis$Temp
  intratime<-rep(1:nclose,nsession)
  datafinal<-data.frame(cbind(databis,session,intratime,chambre,couple,father,mother))
  return(datafinal)
}



test1<-filter(data1,60,19,21,"8","11A","A","11")

contrast<-rep(c("adapt","test","adapt","control"),c(456,57,57,57))
test1B<-subset(test1,contrast!="adapt")
contrast2<-rep(c("test","control"),c(57,57))
attach(test1B)
lm1<-lm(Oxy~intratime*contrast2)
summary(lm1)