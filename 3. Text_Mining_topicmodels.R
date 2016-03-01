
# 請在操作之前將工作路徑換成自己的文件夾
# setwd("E:/ds2015/tm")


# 本地安裝以下兩個R套件，注意在安裝 Rwordseg 之前需要預先安裝 Java 環境，具體方法請參考 『Rwordseg_Vignette_CN.pdf』 文檔
# install.packages("tmcn", repos = "http://R-Forge.R-project.org", type="source") # 不要用李艦大大附的tmcn_0.1-4.zip，下次嚴刑拷問他！！！
library(tmcn)
library(Rwordseg) # 先用RStudio右下方界面從本機硬碟安裝！！
install.packages("tm") # tm0.6-1
library(tm) # 請先從CRAN安裝tm0.6-1 
library(topicmodels)
# setcht()


# 讀入資料文件
dpdata <- read.csv("dianping.csv", fileEncoding  = "UTF-8", stringsAsFactors = FALSE)
head(dpdata)


# 中文斷詞
vec1 <- segmentCN(dpdata$Comment, returnType = "tm")  # 詞向量的格式，只有一組""
head(vec1)

# 建立『文檔-詞條』矩陣
corpus_m <- Corpus(VectorSource(vec1)) # 創建 Corpus 物件
corpus_m <- tm_map(corpus_m, removeWords, c(stopwordsCN())) # 使用 tm 套件提供的預處理函數去除停止詞
corpus_m <- tm_map(corpus_m, removeNumbers) # 使用 tm 套件提供的預處理函數去除數字
# 以下定義分詞的規則（空格）
.strsplit_space_tokenizer <- function(x) {
  unlist(strsplit(as.character(x), "[[:space:]]+"))
}
# 以下建立 DocumentTermMatrix 物件
dtm_m <- DocumentTermMatrix(corpus_m, control = list(tokenize = .strsplit_space_tokenizer, wordLengths = c(1, Inf), removePunctuation = TRUE) )
# 查看該物件：
dtm_m # documents: 19327, terms: 19613

# 主題模型（注意該模型運行時間比較長，15分鐘左右）
system.time(ctm1 <- CTM(dtm_m, k = 3))
terms(ctm1, k=3, 0.02) # Only the terms/topics which are more likely than the threshold are returned.
