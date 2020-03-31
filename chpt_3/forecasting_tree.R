dev.off()
rm(list=ls())
cat("\014")

#----Classification Tree----

#Train a classification tree using electrical load membership data and compare accuracy to actual cluster membership based on artificial occupancy data

#Author(s): Brodie W. Hobson

#Set working directory
setwd("C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 2")

#Load or install required packages
if ("data.table" %in% rownames(installed.packages()) == FALSE) {install.packages("data.table")}
require(data.table)
if ("caret" %in% rownames(installed.packages()) == FALSE) {install.packages("caret")}
require(caret)
if ("rattle" %in% rownames(installed.packages()) == FALSE) {install.packages("rattle")}
require(rattle)
if ("rpart" %in% rownames(installed.packages()) == FALSE) {install.packages("rpart")}
require(rpart)

ind <- read.csv(file = paste0("C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 2\\Data\\JP\\elec_ind.csv"))

words <- cbind.data.frame(ind)

for(i in 1:7){
  
  temp <- unlist(shift(ind,n=i))
  
  words <- cbind.data.frame(words,temp)
  
  rm(temp)
}

words <- words[8:nrow(words),]

colnames(words) <- c("d0","d1","d2","d3","d4","d5","d6","d7")

symbol <- c('h','m','l','L')

#Turn the data frame with observations from numeric to alphabetic
words <- as.data.frame(lapply(words,function(x){x <- symbol[x]}))
  
set.seed()
index <- createDataPartition(y = words$d0, p = 0.7, list = F)

train.set <- words[index,]

test.set <- words[-index,]

dt_f <- as.formula(d0~d1+d2+d3+d4+d5+d6+d7)

mdl_tree <- train(dt_f, data = train.set, method="rpart", parms = list(split = "information"), trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3))

fancyRpartPlot(mdl_tree$finalModel)

CM <- confusionMatrix(data = predict(mdl_tree, test.set), reference = test.set$d0)

accuracy<- as.numeric(CM$overall[1])

nums <- c(1,3,4,2)

ind <- as.vector(unlist(predict(mdl_tree, words)))

ind[which(ind=='h')] <- 1
ind[which(ind=='m')] <- 2
ind[which(ind=='l')] <- 3
ind[which(ind=='L')] <- 4

ind <- as.numeric(ind)

write.csv(ind, "dt_occ_ind.csv", row.names = F)