dev.off()
rm(list=ls())
cat("\014")

#----Artificial Neural Networks----

#Train neural networkss and evaluate their accuracy accross all floors

#BEWARE IF YOU RUN THIS SCRIPT, YOUR COMPUTER WILL BE RUNNING CALCULATIONS FOR OVER THREE HOURS

#Author(s): Brodie W. Hobson

#Set working directory
setwd("C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 1")

#Load or install required packages
if ("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
require(lubridate)
if ("BBmisc" %in% rownames(installed.packages()) == FALSE) {install.packages("BBmisc")}
require(BBmisc)
if ("MASS" %in% rownames(installed.packages()) == FALSE) {install.packages("MASS")}
require(MASS)
if ("boot" %in% rownames(installed.packages()) == FALSE) {install.packages("boot")}
require(boot)
if ("Metrics" %in% rownames(installed.packages()) == FALSE) {install.packages("Metrics")}
require(Metrics)
if ("caret" %in% rownames(installed.packages()) == FALSE) {install.packages("caret")}
require(caret)
if ("matrixStats" %in% rownames(installed.packages()) == FALSE) {install.packages("matrixStats")}
require(matrixStats)
if ("neuralnet" %in% rownames(installed.packages()) == FALSE) {install.packages("nerualnet")}
require(neuralnet)

floors <- c("A","AS","B","BS","C","D")

for (i in 1:length(floors)){
  
  floor.data <- read.csv(file=paste0("C:\\Users\\hobso\\OneDrive - Carleton University\\Working Directory\\Research Project WD\\Phase 1\\Floor ", noquote(floors[i]), ".csv"))
  
  #Convert date information to POSIXct
  floor.data[,1] <- as.POSIXct(floor.data[,1], format = "%Y-%m-%d %H:%M", tz = "GMT")
  
  #Limit model to hours between 6am and 10pm (avoid overfitting to low, unoccupied hours)
  floor.data <- floor.data[which(hour(floor.data[,1]) %in% c(6:20)),]
  
  gt <- normalize(floor.data$groundtruth, method = "range", range = c(0,1))
  wifi <- normalize(floor.data$devices, method = "range", range = c(0,1))
  co2 <- normalize(rowMeans(cbind.data.frame(floor.data[,grep('CO2',colnames(floor.data))])), method = "range", range = c(0,1))
  pir <- normalize((rowSums(cbind.data.frame(floor.data[,grep('CO2',colnames(floor.data))]))/length(floor.data[,grep('CO2',colnames(floor.data))])), method = "range", range = c(0,1))
  light <- normalize(floor.data$light, method = "range", range = c(0,1))
  plug <- normalize(floor.data$plug, method = "range", range = c(0,1))
  elec <- normalize((floor.data$light+floor.data$plug), method = "range", range = c(0,1))
  
  floor.data <- cbind.data.frame(gt,wifi,co2,pir,light,plug,elec)
  
  set.seed()
  index <- createDataPartition(y = nrow(floor.data), p = 0.7, list = F)
  train.set <- floor.data[index,]
  test.set <- floor.data[-index,]
  
  nam <- paste0("test.",noquote(floors[i]))
  assign(nam, test.set)
  
  nam <- paste0("train.",noquote(floors[i]))
  assign(nam, train.set)
  
  nam <- paste0("nfloordata",noquote(floors[i]))
  assign(nam,floor.data)
}

RMSE.fn = function(gt,predicted){
  sqrt(mean((predicted-gt)^2))
}
SSE.fn <- function(gt,predicted){
  sum((predicted - mean(gt))^2)
}
SSR.fn <- function(gt,predicted){
  sum((predicted - gt)^2)
}
R2.fn <- function(SSE,SSR){
  1-(SSR/(SSE+SSR))
}

trains.list <- list(train.A,train.AS,train.B,train.BS,train.C,train.D)
tests.list <- list(test.A,test.AS,test.B,test.BS,test.C,test.D)
floordata.list <- list(nfloordataA,nfloordataAS,nfloordataB,nfloordataBS,nfloordataC,nfloordataD)

#List ofpossible sensor combinations from enumerative combinatorics (clumsy but effective)
combos <- c("wifi","co2","pir","light","plug","elec",
            "wifi+co2","wifi+pir","wifi+light","wifi+plug","wifi+elec","co2+pir","co2+light","co2+plug","co2+elec","pir+light","pir+plug","pir+elec","light+plug",
            "wifi+co2+pir","wifi+co2+plug","wifi+co2+light","wifi+co2+elec","wifi+pir+plug","wifi+pir+light","wifi+pir+elec","wifi+plug+light","co2+pir+plug","co2+pir+light","co2+pir+elec","co2+plug+light","pir+plug+light",
            "wifi+co2+pir+plug","wifi+co2+pir+light","wifi+co2+pir+elec","wifi+co2+plug+light","wifi+pir+plug+light","co2+pir+plug+light",
            "wifi+co2+pir+plug+light")

results.list <- list()

#Specify maximum number of iterations
n=1000

#Specify hidden layer nodes (based on book by J. Heaton)
node_num <- c(rep(2,19),rep(3,13),rep(4,7))

for (j in 1:length(combos)){
  
  predictand.list <- list()
  rmse.list <- list()
  r2.list <- list()
  
  for (i in 1:length(floordata.list)){
    
    #Train ANN (single hidden layer feed-forward with r-bprop, 10e-6 default stopping criteria from neuralnet package)
    mdl <- neuralnet(f = as.formula(paste("gt~", paste0(combos[j]))), data = as.data.frame(trains.list[i]), hidden = node_num[j], output = F, max.rep = n)
    
    for (k in 1:length(floordata.list)){
      
      if(k == i){
        predictand <- compute(mdl, as.data.frame(tests.list[i]))$net.results
      }else{
        predictand <- compute(mdl, as.data.frame(floordata.list[i]))$net.results
      }
      
      rmse <- RMSE.fn(floordata.list[[i]]$gt,predictand)
      SSE <- SSE.fn(floordata.list[[i]]$gt,predictand)
      SSR <- SSR.fn(floordata.list[[i]]$gt,predictand)
      r2 <- R2.fn(SSE,SSR)
      
      predictand.list[[length(predictand.list)+1]] <- predictand
      rmse.list[[length(rmse.list)+1]] <- rmse
      r2.list[[length(r2.list)+1]] <- r2
    }
  }
  
  r2.results.mx <- t(matrix(unlist(r2.list), ncol=length(floors)))
  r2.results.df <- as.data.frame(r2.results.mx, row.names=floors)#predictor
  colnames(r2.results.df) <- floors#predictand
  
  rmse.results.mx <- t(matrix(unlist(rmse.list), ncol=length(floors)))
  rmse.results.df <- as.data.frame(rmse.results.mx, row.names=floors)#predictor
  colnames(rmse.results.df) <- floors#predictand
  
  results <- cbind(r2.results.df,rmse.results.df)[c(1,7,2,8,3,9,4,10,5,11,6,12)]
  
  results.list[[length(results.list)+1]] <- results
}

results.df <- do.call(rbind,results.list)

write.csv(results.df,"ann.predictions.csv")