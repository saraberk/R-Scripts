cort<-read.csv("2016_CORT.csv")
cort<-cort[-12,]

####getting baseline, 10 minute, and 30 minute for males
cort<-subset(cort,cort$Nest.Stage=="C")
males<-subset(cort,cort$Sex=="M")
females<-subset(cort,cort$Sex=="F")
baseline<-subset(males,males$Time=="3")
ten<-subset(males,males$Time=="10")
thirty<-subset(males,males$Time=="30")
type<-as.factor(males$Time)
plot(males$Adj.CORT~type)
baseline<-subset(females,females$Time=="3")
ten<-subset(females,females$Time=="10")
thirty<-subset(females,females$Time=="30")
type<-as.factor(females$Time)
plot(females$Adj.CORT~type)

baseline<-data.frame(baseline$Box,baseline$Date,baseline$Adj.CORT)
colnames(baseline)<-c("Box","Date","Baseline")
ten<-data.frame(ten$Box,ten$Date,ten$Adj.CORT)
colnames(ten)<-c("Box","Date","Ten")
thirty<-data.frame(thirty$Box,thirty$Date,thirty$Adj.CORT)
colnames(thirty)<-c("Box","Date","Thirty")
newdata<-merge(baseline,ten,by="Box",all.x=TRUE,all.y=TRUE)
newdata2<-merge(newdata,thirty,by="Box",all.x=TRUE,all.y=TRUE)
newdata2<-data.frame(newdata2$Box,newdata2$Baseline,newdata2$Ten,newdata2$Thirty)
colnames(newdata2)<-c("Box","Baseline","Ten","Thirty")
write.csv(newdata2,file="FemalesCORT2016_R.csv")

##Max CORT = Max CORT reached
##Integrated baseline= baseline * 30
##Integrated response = (((7*(10min-3min))/2)+(20*(10min-3min))+((20*(30min-10min))/2))
####change 10 to 20 
###integrated response + integrated baseline = itot 
##Rate of Increase to 10 minutes = (10min-3min)/7)
library(plyr)
newdata2[, "Max.CORT"] <- apply(newdata2[, 2:4], 1, max)
newdata2$I.bsl<-newdata2$Baseline*30
newdata2$I.resp<-(((7*(newdata2$Ten-newdata2$Baseline))/2)+
                    (20*(newdata2$Ten-newdata2$Baseline))+((20*(newdata2$Thirty-newdata2$Ten))/2))
newdata2$I.tot<-newdata2$I.bsl+newdata2$I.resp
newdata2$ROI<-(newdata2$Ten-newdata2$Baseline)/7
newdata2$Incr<-(newdata2$Max.CORT-newdata2$Baseline)
newdata2$Neg.FB<-newdata2$Thirty-newdata2$Ten
newdata2$Sex<-"F"
males<-newdata2
newdata2$Incr<-(newdata2$Max.CORT-newdata2$Baseline)
newdata3<-rbind(newdata2,males)
write.csv(newdata3,file="CORTVar_2016.csv")
plot(cort$Chroma.Ind,cort$ROI)

newdata3<-read.csv("MalesCORTVarFC.csv")
data<-read.csv("2017ModelData_Jan18.csv")
newdata4<-merge(data,newdata3,by="Box",all.x=TRUE)
write.csv(newdata4,file="2017ModelData_Jan18.csv")



