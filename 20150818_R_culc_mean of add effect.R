setwd("~/Arbeit/Bildbearbeitung/R/My scripts/Dima screen additive")
library(plyr)
##Load data
data<-read.csv("20150813_ForFrank.csv")

#Set baseline to 0
data$NormToZero<-data$Norm.Mean-1


data_EGF<-subset(data,Stimulus=="EGF")
data_IGF<-subset(data,Stimulus=="IGF")
data_EGF_IGF<-subset(data,Stimulus=="EGF_IGF")


#Mean
Mean_EGF=ddply(subset(data_EGF),.variables=.(time,channel,Sensor),summarize,Mean_EGF=mean(NormToZero,na.rm=T),N_EGF=length(NormToZero),sd_EGF=sd(NormToZero,na.rm=T),.progress="text")
Mean_IGF=ddply(subset(data_IGF),.variables=.(time,channel,Sensor),summarize,Mean_IGF=mean(NormToZero,na.rm=T),N_IGF=length(NormToZero),sd_IGF=sd(NormToZero,na.rm=T),.progress="text")
Mean_EGF_IGF=ddply(subset(data_EGF_IGF),.variables=.(time,channel,Sensor),summarize,Mean_EGF_IGF=mean(NormToZero,na.rm=T),N_EGF_IGF=length(NormToZero),sd_EGF_IGF=sd(NormToZero,na.rm=T),.progress="text")
rm(data_EGF,data_IGF,data_EGF_IGF)

#Calculate standard error of the mean and do conservative error propagation
data_mean=merge(Mean_EGF,Mean_IGF,by=c("time","channel","Sensor"))
data_mean=merge(data_mean,Mean_EGF_IGF)
data_mean$SDE_EGF<-data_mean$sd_EGF/sqrt(data_mean$N_EGF)
data_mean$SDE_IGF<-data_mean$sd_IGF/sqrt(data_mean$N_IGF)
data_mean$SDE_EGF_IGF<-data_mean$sd_EGF_IGF/sqrt(data_mean$N_EGF_IGF)
data_mean$Mean_Additive<-data_mean$Mean_EGF+data_mean$Mean_IGF
data_mean$SDE_Additive<-sqrt((data_mean$SDE_EGF^2+data_mean$SDE_IGF^2))
data_mean<-data_mean[,c("time","channel","Sensor","Mean_EGF","SDE_EGF","Mean_IGF","SDE_IGF","Mean_EGF_IGF","SDE_EGF_IGF","Mean_Additive","SDE_Additive")]

#Merge all into another data.frame
library(reshape)
mdata<-melt(data_mean,id.vars = c("time","channel","Sensor"))
mdata$Stimulus<-mdata$variable
mdata$Stimulus<-gsub("Mean_","",mdata$Stimulus)
mdata$Stimulus<-gsub("SDE_","",mdata$Stimulus)
mdata$Stimulus<-gsub("_","+",mdata$Stimulus)
unique(mdata$Stimulus)
mdata$Stimulus<-factor(mdata$Stimulus,ordered=T,,levels=c("EGF","IGF","EGF+IGF","Additive"))
mdata$variable<-gsub("_[A-Z,a-z]+$","",mdata$variable)
mdata$variable<-gsub("_[A-Z,a-z]+$","",mdata$variable)
cdata<-cast(mdata,formula = Sensor+Stimulus+channel+time~variable)


#Plot everything:
library(ggplot2)
at<-ggplot(data=subset(cdata,time<13000),aes(time/60,Mean,colour=Stimulus,fill=Stimulus))
at<-at+geom_line(alpha=0.25)+facet_wrap(~Sensor,scale="free")+ylab(label="Norm. Mean [a.u.]")+xlab(label = "time [min]")+theme_bw(base_size=8)+theme(legend.position="top",legend.title=element_blank())+scale_x_continuous( breaks=c(0,100,200))
at<-at+geom_smooth(aes(ymin=Mean-SDE,ymax=Mean+SDE),stat="identity",alpha=0.25)
at+scale_fill_brewer(palette = "Set1")+scale_color_brewer(palette = "Set1")
ggsave(file="EGF_IGF_EGFIGF_dotsize_1_1_scale.pdf", units = "mm",width=201,height=162)
