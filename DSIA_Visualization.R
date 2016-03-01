install.packages("stringr")
install.packages("zoo")
install.packages("dygraphs")
install.packages("ggmap")
install.packages("googleVis")
install.packages("dplyr")
install.packages("plotGoogleMaps")
install.packages("xts")
install.packages("devtools")
install.packages("Rcpp")

require(devtools)
install_github('rCharts', 'ramnathv')

path <- "~/Desktop/2015 DSIA workshop/lvr_landAcsv/"
setwd(path)

files <- dir(path, pattern="\\.CSV", recursive=TRUE, full.names=T)
files <- files[grepl("[A-Z]_LVR_LAND_A.CSV", files)]

tables <- lapply(1:length(files), 
                 function(u) read.csv(files[u], fileEncoding = "BIG-5", 
                                      stringsAsFactors = F, 
                                      colClasses = c(rep(NA, 26), rep("NULL", 10))))

corresponda <- read.table("地區對照.txt", 
                          header = T, stringsAsFactors = F)
city <- rep(corresponda[,2], unlist(lapply(tables,nrow)))

tables <- do.call(rbind, tables)
Data <- data.frame("縣市" = city, tables)

load_data <- function(path) {
  files <- dir(path, pattern="\\.CSV", recursive=TRUE, full.names=T)
  files <- files[grepl("[A-Z]_LVR_LAND_A.CSV", files)]
  
  tables <- lapply(1:length(files), 
                   function(u) read.csv(files[u], fileEncoding = "BIG-5", 
                                        stringsAsFactors = F, 
                                        colClasses = c(rep(NA, 26), rep("NULL", 10))))
  
  corresponda <- read.table("地區對照.txt", header = T, stringsAsFactors = F)
  
  city <- rep(corresponda[,2], unlist(lapply(tables,nrow)))
  tables <- do.call(rbind, tables)
  data.frame("縣市" = city, tables)
}

Data <- load_data(path)

str(Data)

library(ggmap)
library(stringr)

location <- "臺北市中山區南京東路二段181~210號"
if(grepl("~", location)){
  Replace<- str_locate(location, "[0-9]+~[0-9]+")
  Replace <- str_sub(location, Replace)[1]
  Replace<- gsub("號", "", Replace)
  No <- as.numeric(unlist(strsplit(Replace, "~")))
  No <- floor((No[2]+No[1])/2)
  No
  location <- str_replace(location, Replace, No)
} 
location

location_process <- function(location){
  if( nchar(location)<3 | grepl("地號", location) | location == ""){
    ""
  }else{
    if(grepl("~", location)){
      Replace<- str_locate(location, "[0-9]+~[0-9]+")
      Replace <- str_sub(location, Replace)[1]
      Replace<- gsub("號", "", Replace)
      No <- as.numeric(unlist(strsplit(Replace, "~")))
      No <- floor((No[2]+No[1])/2)
      location <- str_replace(location, Replace, No)
    } 
    location
  }
}

Data$土地區段位置或建物區門牌 <- sapply(Data$土地區段位置或建物區門牌, location_process)

geocode(location = Data$土地區段位置或建物區門牌[1])

LatLon <- t(sapply(Data$土地區段位置或建物區門牌[1:5], geocode)) 
row.names(LatLon) <- 1:5
LatLon <- as.data.frame(LatLon)

# Data$Lon <- LatLon$lon
# Data$Lat <- LatLon$lat

Data <- read.table("~/Desktop/2015 DSIA workshop/lvr_landAcsv/lvr_land_All.csv", 
                   header = T, sep = ",", stringsAsFactors = F)

Data[1:5, 28:29]

Data$交易年月[1:10]

if(Sys.info()[1]=="Windows"){
  Sys.setlocale("LC_TIME", "C")
}else{
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
}

Data <- Data[Data$交易標的 != "土地", ]

year <- as.numeric(ifelse(nchar(Data$交易年月)==5, str_sub(Data$交易年月, 1, 3), 
                          str_sub(Data$交易年月, 1, 2)))

year <- year+1911

mon <- ifelse(nchar(Data$交易年月)==5, str_sub(Data$交易年月, 4, 5), 
              str_sub(Data$交易年月, 3, 4))

library(zoo)
Data$交易年月 <- as.yearmon(paste0(year,"-", mon))

Data$yearmon <- as.Date(Data$交易年月)
Data$yearmon

library(rCharts)
dTable(Data[1:10, 1:5])

library(dplyr)
library(xts)
library(dygraphs)
Data$Zoning <- ifelse(nchar(Data$都市土地使用分區) > 0, Data$都市土地使用分區, Data$非都市土地使用分區)

colnames(Data)[1] <- "city"
df <- filter(Data, Data$yearmon >as.Date(as.yearmon("2014-01")),Zoning !="") %>%
  mutate(Year = yearmon, Zoning= 都市土地使用分區) %>%
  dplyr::select(Year, Zoning)

df <- aggregate(df$Zoning, list(df$Year), table)
df <- cbind(df[1], df[[2]])

DyGraph <- xts(df[,-1], order.by = df$Group.1) %>% 
  dygraph(main = "土地使用分區") %>% 
  dyRangeSelector %>% 
  dyLegend(width = 800,  showZeroValues = F)

DyGraph

options("scipen"=100, "digits"=4)
Data$SFprice <- Data$總價元/Data$土地移轉總面積平方公尺

Data2 <- Data[!Data$SFprice == Inf, ]
Data2 <- Data2[Data2$yearmon >as.Date(as.yearmon("2014-09")),]

MeanPrice <- aggregate(Data2$SFprice, list(Data2$交易年月), mean)
Trading <- aggregate(Data2$SFprice, list(Data2$交易年月), length)

TradingVolume <- data.frame(MeanPrice, Trading[2])
names(TradingVolume) <- c("Date", "Price", "quantity")

TradingVolume$Date <- as.character(TradingVolume$Date)

require(rCharts)
h <- Highcharts$new()

h$xAxis(categories = as.factor(TradingVolume$Date))

h$yAxis(list(list(title = list(text = '成交量')), 
             list(title = list(text = '每坪單價'),
                  opposite = TRUE)))

h$series(name = '成交量', type = 'column', color = '#4572A7',
         data = TradingVolume$quantity)

h$series(name = '每坪單價', type = 'spline', color = '#89A54E',
         data = as.integer(TradingVolume$Price), yAxis = 1)
h

MeanPrice <- aggregate(Data2$SFprice, list(Data2$交易年月, Data2$city), mean)
Trading <- aggregate(Data2$SFprice, list(Data2$交易年月, Data2$city), length)
TV<- merge(MeanPrice, Trading, c("Group.1", "Group.2"))
names(TV) <- c("Date", "location", "Price", "quantity")
TV <- TV[order(TV$Date),]
TV$Date <- as.character(TV$Date)

TV

n1 <- nPlot(Price ~ Date, 
            group = "location", 
            data = TV, 
            type = "multiBarChart")

n1$chart(showControls = F, margin = list(left = 130))
n1$yAxis(axisLabel = "平均單價", width = 100)
n1$xAxis(axisLabel = "年份")
n1

North <- c("臺北市", "新北市","基隆市","宜蘭縣","桃園市", "桃園縣", "新竹縣","新竹市")
South <- c("嘉義縣","嘉義市","臺南市","高雄市","屏東縣","澎湖縣")
Central <- c("苗栗縣","臺中市","彰化縣","南投縣","雲林縣")
East <- c("花蓮縣", "臺東縣")

TV$Area <- ifelse(TV$location %in% North, "北部",
                  ifelse(TV$location %in% South, "南部", 
                         ifelse(TV$location %in% Central, "中部",
                                ifelse(TV$location %in% East, "東部", "金馬"))))

n2 <- nPlot(Price ~ Date, 
            group = "location", 
            data = TV, 
            type = "multiBarChart")

n2$chart(showControls = F, margin = list(left = 130))
n2$yAxis(axisLabel = "平均單價", width = 100)
n2$xAxis(axisLabel = "年份")
n2$addFilters("Area")
n2

Data2$BuildType <- sapply(strsplit( Data2$建物型態 , "[(]"), "[", 1)
SFdata <- Data2[Data2$city == "臺北市", ]
SFdata <- data.frame(SF = SFdata$建物移轉總面積平方公尺, SFprice = SFdata$SFprice, BuildType = SFdata$BuildType)
SFdata <- SFdata[!is.na(SFdata$BuildType), ]
nPlot(SFprice ~ SF, group = "BuildType", data = SFdata, type = 'scatterChart')

BN <- aggregate(SFdata$BuildType, list(SFdata$BuildType), length)
names(BN) <- c("label", "value")
BN <- BN[order(BN$value),]
BN <- BN[-which(BN$label == "其他"), ]

Pie <- rCharts$new()
Pie$setLib("http://timelyportfolio.github.io/rChartsExtra/d3pie")
Pie$params$chartspec <- list(header = list(title = list(text = "成交建物類型"))
                             ,data = list(content = BN)
                             ,labels = list(lines = list(style = "straight")))
Pie

Bprice <- aggregate(SFdata$SFprice, list(SFdata$BuildType), mean)
names(Bprice) <- c("label", "value")
Bprice<- Bprice[order(Bprice$value, decreasing = T),]
Bprice <- Bprice[-which(Bprice$label == "其他"), ]

n3 <- nPlot(value ~ label, data = Bprice, type = 'multiBarHorizontalChart', width = 800)
n3$chart(showControls = F, margin = list(left = 130))
n3$yAxis(axisLabel = "平均成交單價")
n3

MapData <- Data2[sample(1:nrow(Data2), 50), ]

Tips <- sapply(1:50, function(u) paste0("地址：",MapData[u,4], 
                                        "<BR>面積：",MapData[u, 5], 
                                        "<BR>樓層：",MapData[u, 11],  
                                        "<BR>價格：", MapData[u, 23]))

df <- data.frame(Adress = MapData$土地區段位置或建物區門牌, Tips)

library(googleVis)
Map <- gvisMap(df, "Adress", "Tips", options = list(mapType = 'normal', width = 800, height = 650))

plot(Map)

print(Map, file="GvisMap.html")

Data2$level <- ifelse(Data2$總價元 <= 10000000, "0~1000", 
                      ifelse(Data2$總價元 <= 20000000, "1000~2000", "2000+"))

table(Data2$level)

summary(Data2$lat)
summary(Data2$lon)

Data2 <- Data2[which(Data2$lat < 35 & Data2$lon > 119), ]
Data2 <- Data2[which(Data2$lat !="" | Data2$lon != ""), ]
# Data2[is.na(Data2)] <- ""
MapData2<- Data2[, c(34, 3,4,5, 23, 31:33, 28:29)]
MapData2 <- MapData2[Data2$交易年月==as.yearmon("2015-01"), ]
names(MapData2)

library(plotGoogleMaps)

coordinates(MapData2)<- ~lon+lat
proj4string(MapData2) <- CRS("+init=epsg:4326")
m <- plotGoogleMaps(MapData2, mapTypeId= "ROADMAP")
m2 <- bubbleGoogleMaps(MapData2,zcol='總價元', max.radius=500)
m3 <- mcGoogleMaps(MapData2, zcol='SFprice', filename='Map3.html')
