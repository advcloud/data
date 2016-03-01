
# 請在操作之前將工作路徑換成自己的文件夾
# setwd("E:/ds2015/tm")


# 本地安裝以下兩個R套件，注意在安裝 Rwordseg 之前需要預先安裝 Java 環境，具體方法請參考 『Rwordseg_Vignette_CN.pdf』 文檔
# install.packages("tmcn", repos = "http://R-Forge.R-project.org", type="source") # 不要用李艦大大附的tmcn_0.1-4.zip，下次嚴刑拷問他！！！
library(tmcn)
library(Rwordseg) # 先用RStudio右下方界面從本機硬碟安裝！！！
library(tm) # 先用RStudio右下方界面從本機硬碟安裝舊版tm0.5-10！！！
library(wordcloud)
# library(topicmodels)
library(qdap) # Bridging the Gap Between Qualitative Data and Quantitative Analysis，先用RStudio右下方界面從本機硬碟安裝舊版qdap 2.0.0！！！
# setcht()


# 讀入資料文件
dpdata <- read.csv("dianping.csv", fileEncoding  = "UTF-8", stringsAsFactors = FALSE)
head(dpdata)


# 中文斷詞
vec1 <- segmentCN(dpdata$Comment, returnType = "tm")  # 詞向量的格式，只有一組""
head(vec1)
vec2 <- segmentCN(dpdata$Comment, returnType = "vec", nature = TRUE)  # 包含詞性的格式，很多組""
head(vec2)


# 計算詞頻
head(unlist(vec2))
freq1 <- getWordFreq(unlist(vec2)) # Get the word frequency data.frame.
head(freq1)


# 去除停止詞
vec3 <- removeWords(vec1, stopwordsCN())
freq2 <- getWordFreq(unlist(strsplit(vec3, " ")))
head(freq2)


# 詞雲圖
par(family="STKaiti") # Only for Mac OS
wordcloud(freq2$Word, freq2$Freq, col = rainbow(length(freq2$Freq)), max.words = 300, random.order=FALSE) # 


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
dtm_m	
	

# 關聯分析
findAssocs(dtm_m, "好吃", 0.1) # 列出與『好吃』的關聯度大於 0.1 的詞
findAssocs(dtm_m, "味道", 0.1) # 列出與『味道』的關聯度大於 0.1 的詞


# 情感分析
data(NTUSD) # 加載台灣大學意見詞詞典
# pf1 <- polarity_frame(positives = NTUSD$positive_cht, negatives = NTUSD$negative_cht, pos.weights = 1, neg.weights = -1) 
pf1 <- sentiment_frame(positives = NTUSD$positive_cht, negatives = NTUSD$negative_cht, pos.weights = 1, neg.weights = -1)
dpdata$polarity <- 0
for (i in 1:100) {
	wordsv <- segmentCN(dpdata$Comment[i]) # 各篇comment做斷詞
	dpdata$polarity[i] <- polarity(text.var=wordsv, polarity.frame = pf1)$group[["stan.mean.polarity"]] # Approximate the sentiment (polarity) of text by grouping variable(s). polarity.frame: A dataframe or hash key of positive/negative words and weights.
}

head(dpdata)
head(dpdata$polarity)
sum(dpdata$polarity > 0, na.rm=T)

# 對比詞雲圖
vec4<- sapply(split(vec1, f= dpdata$Taste), paste0, collapse = " ") # 將評論根據口味得分的不同分成5組（0分到4分）
corpus_c <- Corpus(VectorSource(vec4)) # 重建分組後的語料庫
#corpus_c <- tm_map(corpus_c, function(x) removeWords(x,stopwordsCN()))
dtm_c <- TermDocumentMatrix(corpus_c, control = list(wordLengths = c(1, Inf), tokenize = .strsplit_space_tokenizer, removePunctuation = TRUE) )
term.matrix <- as.matrix(dtm_c)
comparison.cloud(term.matrix, max.words=200, random.order=FALSE, rot.per=0.1)

# 主題模型（注意該模型運行時間比較長，15分鐘左右）參見3. Text_Mining_topicmodels
# library(topicmodels)
# ctm1 <- CTM(dtm_m, k = 3)
# terms(ctm1, 3, 0.02)


