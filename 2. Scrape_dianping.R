
# 請在操作之前將工作路徑換成自己的文件夾
# setwd("E:/ds2015/tm")


# 獲取 http://www.dianping.com/ 資料
library(httr) # Tools for Working with URLs and HTTP
library(XML)
shopid = 582525
ipage = 1

# 設定想要抓取的頁面的 URL
strurl <- paste0("http://www.dianping.com/shop/", shopid, "/review_more?pageno=", ipage)
	
# 模擬瀏覽器
HttpHeader <- list(
		"Connection" = "keep-alive",
		"Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
		"User-Agent" = "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/35.0.1916.153 Safari/537.36",
		"Referer" = strurl,
		"Accept-Encoding" = "gzip,deflate,sdch",
		"Accept-Language" = "zh-CN,zh;q=0.8,en;q=0.6"
)

# 抓取網頁信息
htmlcon <- GET(url=strurl, config=add_headers(.headers = unlist(HttpHeader))) # GET a url. "response"
htmlstr <- content(htmlcon, "text", encoding = "UTF-8") # Extract content from a request. "character"

# 將網頁的文字資料轉換成 DOM 樹
pagetree <- htmlParse(htmlstr, asText = TRUE, encoding = "UTF-8")

# 解析網頁信息
tmp.id <- sapply(getNodeSet(pagetree, "//body//div[@class = 'comment-list']/ul/li"), xmlGetAttr, "data-id") # 抓點評者id
tmp.comment <- sapply(getNodeSet(pagetree, "//body//div[@class = 'comment-list']//div[@class = 'comment-txt']"), xmlValue) # 抓點評文字
tmp.price <- sapply(getNodeSet(pagetree, "//body//div[@class = 'comment-list']//div[@class = 'content']/div[@class = 'user-info']"), xmlValue) # 抓價格
tmp.time <- sapply(getNodeSet(pagetree, "//body//div[@class = 'comment-list']//span[@class = 'time']"), xmlValue) # 抓點評時間
tmp.username <- sapply(getNodeSet(pagetree, "//body//div[@class = 'comment-list']//p[@class = 'name']"), xmlValue) # 抓點評時間
tmp.score <- sapply(getNodeSet(pagetree, "//body//div[@class = 'comment-list']//div[@class = 'comment-rst']"), xmlValue) # 抓點評分數
tmp.userrank <- sapply(getNodeSet(pagetree, "//body//div[@class = 'comment-list']//p[@class = 'contribution']/span"), xmlGetAttr, "class") # 抓使用者rank

tmp.comment <- gsub("\\s+", "", tmp.comment) # 去掉\n\n                \n等
tmp.price <- as.numeric(gsub("^[^0-9]*", "", sapply(strsplit(gsub("\\s+", "", tmp.price), split = "|", fixed = TRUE), "[[", 1))) # gsub("^[^0-9]*", "", ...不是數字的都移除掉！
tmp.time <- sapply(strsplit(tmp.time, split = "\\s+"), "[[", 1)
tmp.time[nchar(gsub("[0-9]", "", tmp.time)) < 2] <- paste0(format(Sys.time(), "%y"), "-", tmp.time[nchar(gsub("[0-9]", "", tmp.time)) < 2]) # "06-03"轉成"15-06-03"
tmp.score <- strsplit(gsub("[^0-9]", "", gsub("\\s+", "", tmp.score)), split = "") # 先把tmp.score中\n\n                \n等去掉，再將非數字的去掉，最後把口味、環境、服務等分數分開
tmp.taste <- as.numeric(sapply(tmp.score, "[[", 1)) # 取出口味分數
tmp.envir <- as.numeric(sapply(tmp.score, "[[", 2)) # 取出口味分數
tmp.service <- as.numeric(sapply(tmp.score, "[[", 3)) # 取出口味分數
tmp.userrank <- as.numeric(gsub("[^0-9]", "", tmp.userrank)) # 取出使用者rank

# 生成最終的 data.frame
OUT <- data.frame(
		StoreID = shopid,
		ID  = tmp.id,
		Comment = tmp.comment,
		Date = tmp.time,
		Price = tmp.price,
		Envir = tmp.envir,
		Service = tmp.service,
		Taste = tmp.taste,
		Sum = mean(c(tmp.envir, tmp.service, tmp.taste)),
		Sen = 0,
		stringsAsFactors = FALSE
)

# 查看抓取到的資料
OUT
names(OUT) # 變數與dianping.csv不同

# save(OUT, file="Scrape_dianping_out.RData")
