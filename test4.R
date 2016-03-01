# 加载工具包
 library(plyr)
 library(quantmod)
 library(TTR)
 library(ggplot2)
 library(scales)

 download<-function(stock,from="2010-01-01"){ # 下载数据并保存到本地
   df<-getSymbols(stock,from=from,env=environment(),auto.assign=FALSE)  # 下载数据
   names(df)<-c("Open","High","Low","Close","Volume","Adjusted")
   write.zoo(df,file=paste(stock,".csv",sep=""),sep=",",quote=FALSE) # 保存到本地文件
  }

 read<-function(stock){ # 从本地文件读数据
    as.xts(read.zoo(file=paste(stock,".csv",sep=""),header = TRUE,sep=",",                      format="%Y-%m-%d"))
  }

 stock<-"IBM" # 下载IBM的股票行情数据
 download(stock,from='2010-01-01')
 IBM<-read(stock) # 把数据加载到内存

 class(IBM) # 查看数据类型
#[1] "xts" "zoo"

 head(IBM) # 查看前6条数据
 
  ma<-function(cdata,mas=c(5,20,60)){ 
    ldata<-cdata
        for(m in mas){    ldata<-merge(ldata,SMA(cdata,m))          }
        ldata<-na.locf(ldata, fromLast=TRUE)
        names(ldata)<-c('Value',paste('ma',mas,sep=''))
        return(ldata)
    }
 
  drawLine<-function(ldata,titie="Stock_MA",sDate=min(index(ldata)),eDate=  max(index(ldata)),out=FALSE){ 
        g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
        g<-g+geom_line()
        g<-g+geom_line(aes(colour=Series),data=fortify(ldata[,-1],melt=TRUE))
        g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks ("2 months"),limits = c(sDate,eDate))
       g<-g+xlab("") + ylab("Price")+ggtitle(title)
       if(out) ggsave(g,file=paste(titie,".png",sep=""))else g
      }
 
 # 运行程序
  cdata<-IBM['2010/2012']$Close # 取收盘价
  title<-"Stock_IBM" # 图片标题
  sDate<-as.Date("2010-1-1") # 开始日期
  eDate<-as.Date("2012-1-1") # 结束日期
 # ma(cdata,c(5,20,60))
  
  ldata<-ma(cdata,c(5,20,60)) # 选择滑动平均指标
  drawLine(ldata,title,sDate,eDate) # 画图，如图2-7所示
   ldata<-ma(cdata,c(20))  #选择滑动平均指标
   drawLine(ldata,title,sDate,eDate) #画图
   
   drawPoint<-function(ldata,pdata,titie,sDate,eDate){
        g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
        g<-g+geom_line()
        g<-g+geom_line(aes(colour=Series),data=fortify(ldata[,-1],melt=TRUE))
        g<-g+geom_point(aes(x=Index,y=Value,colour=Series),data=fortify(pdata,melt=TRUE))
        g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks("2 months"),limits = c(sDate,eDate))
        g<-g+xlab("") + ylab("Price")+ggtitle(title)
        g
      }
   
   
   pdata<-merge(ldata$ma20[which(ldata$Value-ldata$ma20>0)],ldata$ma20[which   (ldata$Value-ldata$ma20<0)])
    names(pdata)<-c("down","up")
    pdata<-fortify(pdata,melt=TRUE)
    pdata<-pdata[-which(is.na(pdata$Value)),]
   
    head(pdata)
   
    drawPoint(ldata,pdata,title,sDate,eDate) # 画图，如图2-9所示
     Signal<-function(cdata,pdata){ # 交易信号
           tmp<-''
           tdata<-ddply(pdata[order(pdata$Index),],.(Index,Series),function(row){
                 if(row$Series==tmp) return(NULL)
                 tmp<<-row$Series
             })
           tdata<-data.frame(cdata[tdata$Index],op=ifelse(tdata$Series=='down','B','S'))
           names(tdata)<-c("Value","op")
           return(tdata)
       }
    
     tdata<-Signal(cdata,pdata)
     tdata<-tdata[which(as.Date(row.names(tdata))<eDate),]
     head(tdata)
     nrow(tdata) # 交易记录
     
      trade<-function(tdata,capital=100000,position=1,fee=0.00003){   
       # 交易信号，本金，持仓比例，手续费比例
            amount<-0 # 持股数量
            cash<-capital # 现金
       
              ticks<-data.frame()
              for(i in 1:nrow(tdata)){
                    row<-tdata[i,]
                    if(row$op=='B'){
                          amount<-floor(cash/row$Value)
                          cash<-cash-amount*row$Value
                      }
           
                      if(row$op=='S'){
                            cash<-cash+amount*row$Value
                            amount<-0
                        }
           
                      row$cash<-cash # 现金
                      row$amount<-amount # 持股数量
                      row$asset<-cash+amount*row$Value # 资产总值
                      ticks<-rbind(ticks,row)
                  }
         
                ticks$diff<-c(0,diff(ticks$asset)) # 资产总值差
           
                  # 赚钱的操作
                  rise<-ticks[c(which(ticks$diff>0)-1,which(ticks$diff>0)),]
                  rise<-rise[order(row.names(rise)),]
             
                    # 赔钱的操作
                    fall<-ticks[c(which(ticks$diff<0)-1,which(ticks$diff<0)),]
                    fall<-fall[order(row.names(fall)),]
               
                      return(list(
                           ticks=ticks,
                            rise=rise,
                            fall=fall
                        ))
               }
     
      result1<-trade(tdata,100000)
     
     # 查看每笔交易
      head(result1$ticks)
     
      # 盈利的交易
       head(result1$rise)
       # 亏损的交易
       head(result1$fall)
      # 查看最后的资金情况。
       tail(result1$ticks,1)
      # 最后，资金剩余96363.76美元，也就是我们亏了3636.24美元
       
        drawCash<-function(ldata,adata){
            g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
            g<-g+geom_line()
            g<-g+geom_line(aes(x=as.Date(Index), y=Value,colour=Series),data=fortify
                            (adata,melt=TRUE))
            g<-g+facet_grid(Series ~ .,scales = "free_y")
            g<-g+scale_y_continuous(labels = dollar)
            g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks("2 months"),
                               limits = c(sDate,eDate))
            g<-g+xlab("") + ylab("Price")+ggtitle(title)
            g
          }
       
       # 现金流量
        adata<-as.xts(result1$ticks[which(result1$ticks$op=='S'),]['cash'])
        drawCash(ldata,adata) # 画图
       
    ###################二条均线策略模型########################################
         ldata<-ma(cdata,c(5,20)) # 选择滑动平均指标
         drawLine(ldata,title,sDate,eDate) # 画图
        
          pdata<-merge(ldata$ma20[which(ldata$ma5-ldata$ma20>0)],ldata$ma20[which(ldata$ma5-
                                                                                     ldata$ma20<0)])
          names(pdata)<-c("down","up")
          pdata<-fortify(pdata,melt=TRUE)
          pdata<-pdata[-which(is.na(pdata$Value)),]
        #  以散点覆盖20日均线，红色点为买入持有，紫色点为卖出空仓，如图2-12所示
          head(pdata)
          drawPoint(ldata,pdata,title,sDate,eDate) # 画图，如图2-12所示。  
           tdata<-Signal(cdata,pdata)
           tdata<-tdata[which(as.Date(row.names(tdata))<eDate),]
           head(tdata)      
           nrow(tdata)         # 交易记录 
           result2<-trade(tdata,100000)
           
           # 查看每笔交易
            head(result2$ticks)
            # 盈利的交易
             head(result2$rise)
             # 亏损的交易
              head(result2$fall)
             # 查看最后的资金情况。
              
              tail(result2$ticks,1)
               adata<-as.xts(result2$ticks[which(result2$ticks$op=='S'),]['cash'])
               drawCash(ldata,adata)
               # 盈利的交易
               
                rise<-merge(as.xts(result1$rise[1]),as.xts(result2$rise[1]))
                names(rise)<-c("plan1","plan2")
               
               # 查看数据情况
                rise   
                # 均线图+交易区间
                
                 drawRange<-function(ldata,plan,titie="Stock_2014",sDate=min(index(ldata)),
                                      eDate=max(index(ldata)),out=FALSE){
                     g<-ggplot(aes(x=Index, y=Value),data=fortify(ldata[,1],melt=TRUE))
                     g<-g+geom_line()
                     g<-g+geom_line(aes(colour=Series),data=fortify(ldata[,-1],melt=TRUE))
                     g<-g+geom_rect(aes(NULL, NULL,xmin=start,xmax=end,fill=plan),ymin = yrng[1], 
                                     ymax = yrng[2],data=plan)
                     g<-g+scale_fill_manual(values =alpha(c("blue", "red"), 0.2))
                     g<-g+scale_x_date(labels=date_format("%Y-%m"),breaks=date_breaks
                                        ("2 months"),limits = c(sDate,eDate))
                     g<-g+xlab("") + ylab("Price")+ggtitle(title)
                  
                       if(out) ggsave(g,file=paste(titie,".png",sep=""))
                     else g
                   }
                
                # 盈利区间
                 yrng <-range(ldata$Value)
                 plan1<-as.xts(result1$rise[c(1,2)])
                 plan1<-data.frame(start=as.Date(index(plan1)[which(plan1$op=='B')]),end=
                                      as.Date(index(plan1)[which(plan1$op=='S')]),plan='plan1')
                 plan2<-as.xts(result2$rise[c(1,2)])
                 plan2<-data.frame(start=as.Date(index(plan2)[which(plan2$op=='B')]),end=
                                      as.Date(index(plan2)[which(plan2$op=='S')]),plan='plan2')
                
                 plan<-rbind(plan1) # plan1的盈利区间
                 drawRange(ldata,plan,title,sDate,eDate) # 画图
              #  plan1的盈利区间，如图2-14所示。
                 plan<-rbind(plan1,plan2) # 合并plan1和plan2的盈利区间
                 drawRange(ldata,plan,title,sDate,eDate) # 画图 
                
                
                
                
                
                
               