

add_col_to_df<-function(df,col_name,colvar_init=NA,Vector,data_col="Experiment.ID",Vectornames=NULL,create_column=T){
  if(create_column)df[,col_name]<-colvar_init
  df[,col_name]<-as.character(df[,col_name])
  if(length(Vectornames)==0)Vectornames<-Vector
  count=1
  for(i in Vector){
    print(i)
    x<-Vectornames[count][1]
    df[,col_name][grep(i,df[,data_col])]<-x
    count=count+1
  }
  df[,col_name]<-factor(df[,col_name],ordered=T,levels=)
  return(df)
}


runmean<-function(array,places=1,plot=F){
        if(length(na.omit(array))>3&places<length(na.omit(array))){
                sarray<-NULL
                for(i in c(1:length(array))){
                        mean=0
                        counter=0
                        start=i-places
                        if(start<1)start=1
                        end=i+places
                        if(end>length(array))end=length(array)
                        sarray[i]<-mean(array[start:end],na.rm=T)
                }
                if(plot){
                        plot(array,type="l")
                        points(c(1:length(sarray)),sarray,col="red",type="l")
                }
                return(sarray)
        } 
        else return(array)
}
summarise_traces<-function(x=c(1:length(y)),y,Stimulusafterframe,Equilibrationtime=0,stopafterframe=0,NAthreshold=0.1){
        names.result<-c("Mean.response","Mean.amplitude","Mean.change","Max.response","Max.amplitude","Max.change","Min.response","Min.amplitude","Min.change","Baseline","Baseline.StdDev","Fit.StdDev.baseline","Baseline.slope","Fit.StdDev.response","Response.slope","Response.StdDev","Fit.StdDev.all","Allfit.slope","NA.length")
        results<-rep(NA,length(names.result))
        names(results)<-names.result
        if(stopafterframe==0){
                stopafterframe=length(x)
        }
        if(length(Stimulusafterframe)>1)Stimulusafterframe=Stimulusafterframe[1]
        NA.length<-length(which(is.na(y)))
        if(NA.length/length(y)>NAthreshold)return(results)
        Baseline<-mean(y[1:Stimulusafterframe],na.rm=T)
        fit.Baseline<-lm(y[1:Stimulusafterframe]~x[1:Stimulusafterframe])
        fit.Response<-lm(y[(Stimulusafterframe+Equilibrationtime):stopafterframe]~x[(Stimulusafterframe+Equilibrationtime):stopafterframe])
        fit.all<-lm(y~x)
        Fit.StdDev.all<-as.numeric(mean(abs(fit.all$residuals),na.rm=T))
        Fit.StdDev.baseline<-as.numeric(mean(abs(fit.Baseline$residuals),na.rm=T))
        Fit.StdDev.response<-as.numeric(mean(abs(fit.Response$residuals),na.rm=T))
        Allfit.slope<-as.numeric(fit.all$coefficients[2])
        Baseline.slope<-as.numeric(fit.Baseline$coefficients[2])
        Response.slope<-as.numeric(fit.Response$coefficients[2])
        Baseline.StdDev<-sd(y[1:Stimulusafterframe],na.rm=T)
        Response.StdDev<-sd(y[(Stimulusafterframe+Equilibrationtime):stopafterframe],na.rm=T)
        y.response<-y[(Stimulusafterframe+Equilibrationtime):stopafterframe]
        y.response<-runmean(y.response,1)
        Mean.response<-(mean(y.response,na.rm=T)-Baseline)/Baseline.StdDev
        Mean.amplitude<-(mean(y.response,na.rm=T)-Baseline)
        Mean.change<-(Mean.amplitude/Baseline)*100
        Max.response<-(max(y.response,na.rm=T)-Baseline)/Baseline.StdDev
        Max.amplitude<-(max(y.response,na.rm=T)-Baseline)
        Max.change<-(Max.amplitude/Baseline)*100
        Min.response<-(min(y.response,na.rm=T)-Baseline)/Baseline.StdDev
        Min.amplitude<-(min(y.response,na.rm=T)-Baseline)
        Min.change<-(Min.amplitude/Baseline)*100
        results<-c(Mean.response,Mean.amplitude,Mean.change,Max.response,Max.amplitude,Max.change,Min.response,Min.amplitude,Min.change,Baseline,Baseline.StdDev,Fit.StdDev.baseline,Baseline.slope,Fit.StdDev.response,Response.slope,Response.StdDev,Fit.StdDev.all,Allfit.slope,NA.length)
        names(results)<-names.result
        return(results)
}

outlier_removal<-function(df,channels=c("C1 Marker region-Background-ratio"),columns=c("Baseline.StdDev"
),p=T,f=2,direction="both",method="quantile"){
        ids=NULL
        Labteks<-as.character(unique(df$Labtek))
        for(lt in Labteks){
                data_Sensors<-subset(df,Labtek==lt);
                Sensors<-as.character(unique(data_Sensors$Sensor))
                for(sn in Sensors){
                        data_channels<-subset(data_Sensors,Sensor==sn);
                        channels<-as.character(unique(data_channels$channel))
                        for(ch in channels){
                                for(cols in columns){
                                        iids<-NULL
                                        sub_df<-subset(data_channels,channel==ch&Sensor==sn&Labtek==lt);
                                        print(sn)
                                        print(lt)
                                        values<-sub_df[,cols]
                                        if(p)(hist(values,breaks=100,main=paste(cols,sn)))
                                        if(method=="IQR"){
                                                limu=quantile(values,0.75,na.rm=T)+f*IQR(values,na.rm=T)
                                                liml=quantile(values,0.25,na.rm=T)-f*IQR(values,na.rm=T)
                                        }else{
                                                if(method=="quantile"){
                                                        limu=quantile(values,0.75,na.rm=T)+f*abs(quantile(values,0.95,na.rm=T)-median(values,na.rm=T))
                                                        liml=quantile(values,0.25,na.rm=T)-f*abs(median(values,na.rm=T)-quantile(values,0.05,na.rm=T))
                                                }else{
                                                        limu=quantile(values,0.75,na.rm=T)+f*IQR(values,na.rm=T)
                                                        liml=quantile(values,0.25,na.rm=T)-f*IQR(values,na.rm=T)
                                                }
                                        }
                                        if(p)(abline(v=c(liml,limu)))
                                        if(direction=="both")iids=as.data.frame(unique(sub_df$ROI.ID[which(values>=limu|values<=liml)]))
                                        if(direction=="low")iids=as.data.frame(unique(sub_df$ROI.ID[which(values<=liml)]))
                                        if(direction=="up")iids=as.data.frame(unique(sub_df$ROI.ID[which(values>=limu)]))
                                        names(iids)[1] <- "ROI.ID"
                                        if(length(iids$ROI.ID)>0)iids$channel<-paste(ch,cols)
                                        ids<-rbind(ids,iids)
                                        print(paste(ch,"-",cols,"-","ll:",liml,"ul:",limu,"Sensor-",sn))
                                }
                        }
                }
        }  
        return((ids))
}

identify_outlier_traces<-function(x=c(1:length(y)),y,Stimulusafterframe=10,Equilibrationtime=0,stopafterframe=0,NAthreshold=0.1,z.threshold,median.distance=3){
  names.result<-c("z.max","z.min","points.over.threshold","class","No.of.NAs")
  results<-rep(NA,length(names.result))
  names(results)<-names.result
  if(stopafterframe==0){
    stopafterframe=length(x)
  }
  if(length(Stimulusafterframe)>1)Stimulusafterframe=Stimulusafterframe[1]
  NA.length<-length(which(is.na(y)))
  if(NA.length/length(y)>NAthreshold){
    return(results)
  }
  baseline<-y[1:Stimulusafterframe]
  nas.b<-length(which(is.na(baseline)))
  baseline<-baseline[!is.na(baseline)]
  baseline<-baseline-runmed(baseline,median.distance)
  q.b<-quantile(baseline,probs=c(0.05,0.95),na.rm=T)
  main.b<-baseline[baseline<=q.b[2]&baseline>=q.b[1]]
  z.b<-(baseline-mean(main.b,na.rm=T))/sd(main.b,na.rm=T)
  
  response<-y[(Stimulusafterframe+Equilibrationtime):length(y)]
  nas<-length(which(is.na(response)))
  nas<-nas+nas.b
  response<-response[!is.na(response)]
  response<-response-runmed(response,median.distance)
  q<-quantile(response,probs=c(0.05,0.95),na.rm=T)
  main<-response[response<=q[2]&response>=q[1]]
  z<-(response-mean(main,na.rm=T))/sd(main,na.rm=T)
  z<-c(z.b,z)
  z.min<-min(z,na.rm=T)
  z.max<-max(z,na.rm=T)
  class="no outlier"
  peaks<-length(which(z>z.threshold|z<(-1*z.threshold)))
  if(max(c(abs(z.min),abs(z.max)))>z.threshold){
    class="outlier"
  }
  z.max<-max(c(abs(z.min),abs(z.max)))
  results<-c(z.max,z.min,peaks,class,nas)
  names(results)<-names.result
#   if(class=="outlier"){
#     plot(response,type="l")
#     print(results)
#     oscil<-readline("Press Esc if you want to leave or enter to continue")
#     }
  return(results)
}

library(ggplot2)
library(plyr)
library(reshape)
