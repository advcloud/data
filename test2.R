library(plyr)
library(quantmod)
library(TTR)
library(ggplot2)
library(scales)

#下载数据
 #download<-function(stock,from="2010-01-01"){
 # + df<-getSymbols(stock,from=from,env=environment(),auto.assign=FALSE)  #下载数据
 # + names(df)<-c("Open","High","Low","Close","Volume","Adjusted")
 # + write.zoo(df,file=paste(stock,".csv",sep=""),sep=",",quote=FALSE) #保存到本地
 # }

#本地读数据
 #read<-function(stock){  
  #+  as.xts(read.zoo(file=paste(stock,".csv",sep=""),header = TRUE,sep=",", format="%Y-%m-%d"))
 # }

# stock<-"IBM"
 #download(stock,from='2010-01-01')
 #IBM<-read(stock)
 IBM = getSymbols("IBM", from='2010-01-01', to='2012-1-1', auto.assign = FALSE)
# 查看数据类型
 class(IBM)
#[1] "xts" "zoo"

# 查看前6条数据
 head(IBM)
 tail(IBM)
 chartSeries(IBM,TA=NULL, theme = "white")
 chartSeries(IBM,TA = "addVo(); addSMA(); addEnvelope();addMACD(); addROC()", theme = "white")
 #移动平均
  ma<-function(cdata,mas=c(5,20,60)){ 
   +     ldata<-cdata
   +     for(m in mas){
     +         ldata<-merge(ldata,SMA(cdata,m))
          }
   +     ldata<-na.locf(ldata, fromLast=TRUE)
   +     names(ldata)<-c('Value',paste('ma',mas,sep=''))
   +     return(ldata)
    }
 
 # 均线图
  drawLine<-function(ldata,titie="Stock_MA",sDate=min(index(ldata)),eDate=max(index(ldata)),out=FALSE){
   +     g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
   +     g<-g+geom_line()
   +     g<-g+geom_line(aes(colour=Series),data=fortify(ldata[,-1],melt=TRUE))
   +     g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks("2 months"),limits = c(sDate,eDate))
   +     g<-g+xlab("") + ylab("Price")+ggtitle(title)
        
     +     if(out) ggsave(g,file=paste(titie,".png",sep=""))
     }
 
 # 运行程序
  cdata<-IBM['2010/2012']$Close
  title<-"Stock_IBM" #图片标题
  sDate<-as.Date("2010-1-1") #开始日期
  eDate<-as.Date("2012-1-1") #结束日期
 
  ldata<-ma(cdata,c(5,20,60))  #选择滑动平均指标
  drawLine(ldata,title,sDate,eDate) #画图
  drawLine(ma(cdata,c(5,20,60)),title,sDate,eDate) #画图
  addSMA(n = 5,col = 2)
  addSMA(n = 60, col = 5)
  addSMA(n = 20, col = 2)
  
  chartSeries(IBM,TA=NULL, theme = "white")
  Cl5SMA = SMA(Cl(IBM),n = 5)
  Cl60SMA = SMA(Cl(IBM),n = 60)
  
  
  addTA(Cl5SMA - Cl60SMA,col=4)
  addTA(sign(Cl5SMA - Cl60SMA),col=5)
  
  Cl20SMA = SMA(Cl(IBM),n = 20)
  avg =  rowMeans(HLC(IBM))
  AvgPrice = newTA(FUN=avg)
  ?addTA
  ?newTA
  ?rowMeans
  addTA(sign(Cl20SMA - rowMeans(HLC(IBM))),col=4)
  addTA(diff(sign(Cl20SMA - rowMeans(HLC(IBM)))),col=8)
  
  chartSeries(IBM,TA=NULL, theme = "white")
  Cl20SMA = SMA(Cl(IBM),n = 20)
  
  avg =  rowMeans(HLC(IBM))
  addSMA(n = 20, col = 2)
  TDnDates <- index(Cl20SMA)[which(diff(sign(Cl20SMA - avg)) > 0 )]
  TUpDates <- index(Cl20SMA)[which(diff(sign(Cl20SMA - avg)) < 0 )]
  class(TDnDates)
  head(TDnDates)
  head(TUpDates)
  liftRatio = 0.02
  addTA(Hi(IBM)[TDnDates]*(1+liftRatio),on=1,type="p",col=3,pch=25,bg="green")
  addTA(Lo(IBM)[TUpDates]*(1-liftRatio),on=1,type="p",col=2,pch=24,bg="red")
  
 
  
       which(diff(sign(Cl20SMA - avg)) < 0 )
       which(diff(sign(Cl20SMA - avg)) > 0 )
       head(avg)
       nrow(avg)
       class(avg)
  