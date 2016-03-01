
# 請在操作之前將工作路徑換成自己的文件夾
# setwd("E:/ds2015/tm")


# 獲取 facebook 資料
# 其中 xxxx 表示申請的賬號信息，具體操作過程請參考 『facebook開發者申請.pptx』
library(Rfacebook)
# fb_oauth <- fbOAuth(app_id="446163392232564", app_secret="b47b09deed67ca69adc84ac4f01fbb42")

token <- "CAACEdEose0cBAIVZCnJhcjQIARFEpGw9TcSvRR6QZACVQjkUmoY2XgY5LtuAYLGLkZAI3PEQi8maff6dxrX3JKNKjmJNRUxhUgFJyFJrwbKLgq7Ig8JB9JPasobywJ3NyOqyx5WpqCJeJUHov9aedStJMbWj7zsbE4ugHzZA53WWXSgtAL5odUiyRmpDhLUsZC9ZAyA0HcRDNfh2fvvWhB"

# 獲取頁面信息
# page1 <- getPage(page="thecharmingclub", token = fb_oauth, feed=TRUE)
page1 <- getPage(page="thecharmingclub", token = token, feed=TRUE)

list1 <- list()
# 獲取 Post 和評論信息
for (i in 1:10) {
# 	list1[[i]] <- getPost(post = page1$id[i], token = fb_oauth, n = 5000)
  list1[[i]] <- getPost(post = page1$id[i], token = token, n = 5000)
	Sys.sleep(abs(rnorm(1, 1, 0.5)))
}

# 資料結構的理解
names(list1[[1]]) # "post"     "likes"    "comments"
class(list1[[1]]$post) # "data.frame"
dim(list1[[1]]$post) # 1 10
head(list1[[1]]$post)
list1[[1]]$post

list2 <- sapply(list1, FUN = function(X) {res <- X$comments;res$post_id <- X$post$id;res}) # (6 from comments + 1) * 10 = 70 elements

list3 <- list2[sapply(list2, length) > 1]

comment1 <- do.call("rbind", list3)
dim(comment1) # 70 * 211

# save(page1, list1, list2, list3, comment1, file="FB.RData")
